-- {-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | A simple prototype of a dynamic scheduling algorithm.

module Main where

import Data.Word
import Data.Map as M
import Data.Set as S
import Data.List as L
import Debug.Trace

--------------------------------------------------------------------------------

type DevID = String
type TaskID = Int

-- | Which memory space are we in, this can be any kind of unique ID.
data MemID = CPU | GPU1 | GPU2    deriving (Show,Eq,Ord,Read)

data Device = Device { did :: DevID   
                     , mem :: MemID   -- ^ From which memory does this device consume and produce results?
                     , prio :: Double -- ^ A bias factor, tend towards using
                                      -- this device if it is higher priority.
                     }
              deriving (Show,Eq,Ord)

--------------------------------------------------------------------------------

-- | A task could have compute cost / output size, but we don't know
-- those till later, so use runTask.
data Task = Task { tid :: TaskID }
               deriving (Show,Eq,Ord,Read)

type Time = Double
type Size = Word -- ^ Say, in bytes

data TaskGraph = TaskGraph { tasks :: [Task]
                           , depends :: Map TaskID (Set TaskID) }
               deriving (Show,Eq,Ord,Read) 

--------------------------------------------------------------------------------

-- | The events that the scheduling loop responds to.
data Event = DeviceFree DevID (Maybe (TaskID, Size))
             -- ^ returns the size of the array just computed by that device, if any

-- | The state of the scheduler at any point in time.
data Sched = Sched
             { freeDevs    :: Set DevID         -- ^ Free devices
             , workingDevs :: Map DevID TaskID  -- ^ Busy devices, and what they're working on.
             , arrays :: Map TaskID (Size, Set MemID)
                         -- ^ Resident arrays, indexed by the taskID that produced them.
                         --   Note, a given array can be on more than one device.
                         --   Every taskId eventually appears in here, so it is also our "completed" list.
             , waiting     :: Set TaskID -- ^ Work waiting on something.
             , outstanding :: Set TaskID -- ^ Work currently executing.
             }
             deriving (Show,Eq,Ord,Read)

-- | The actions a scheduler can take:
data Action = RunKernel DevID Task         deriving (Show,Eq,Ord,Read)
data EventStream = Done | Next [Event] ([Action] -> EventStream)

-- | A log of scheduler events so we can watch what happens.
data Log = NoDevices | NoReadyTasks | InvokeCont [Action]
         | Final Sched | Update Sched | BufferAction Action 
  deriving (Show,Eq,Ord,Read) 

-- | This is the collection of all arrays stored on all memories.
--   TODO: we could model deallocation by removing `MemID`s from the set.
type ArrayStore = Map TaskID (Size, Set MemID)

-- | We process a stream of events and we return a stream of updated Sched states.
scheduler :: ArrayStore -> [Device] -> TaskGraph -> EventStream -> [Log]
scheduler initArrays allDevs (TaskGraph allTasks depends) events =
  -- Tasks with no dependencies start out ready:
   let 
       initSched = Sched{ freeDevs=S.fromList (L.map did allDevs)
                        , workingDevs = M.empty
                        , arrays      = initArrays
                        , waiting     = S.fromList (L.map tid allTasks)
                        , outstanding = S.empty
                        }
   in Update initSched :
      loop initSched events []
  where
    devMap = M.fromList [ (did d, d) | d <- allDevs ]
    tskMap = M.fromList [ (tid t, t) | t <- allTasks ]

    -- Based on what arrays are available in memory, compute which ops are ready to go.
    partitionReady arrays tasks = 
      let available = M.keysSet arrays 
          ready = [ tid | tid <- S.toList tasks, S.isSubsetOf (depends!tid) available ]
          waiting = S.difference (tasks) (S.fromList ready)
      in (ready,waiting)

    -- This is the case where we are finished with all our work, so we terminate things:
    loop Sched{..} _ _ | S.null outstanding && S.null waiting = [Final Sched{..}]
    loop fin Done _ = [Final fin] -- Runtime terminates our run.
    
    -- Messages from the runtime are processed, now it's our turn to schedule something:
    loop Sched{..} (Next [] kont) acts =
      -----------------------------------------
      -- The heart of the scheduling algorithm:
      -----------------------------------------
      let (ready,notready) = partitionReady arrays waiting in
      case (S.toList freeDevs, ready) of
        ([],_) -> NoDevices : InvokeCont acts : 
                  loop Sched{..} (kont acts) []
        (_,[]) -> NoReadyTasks : InvokeCont acts :
                  loop Sched{..} (kont acts) []
        -- If we knew at least a little about how big the tasks were
        -- in advance, then we might at least sort them...
        (freels, tsk1:restReady) ->
          let candidates = [ (dev, computeAffinity tsk1 (devMap#dev) arrays) | dev <- freels ]
              first:rest = sortBy (\ a b -> compare (snd a) (snd b)) candidates
              -- Equivalence class.  Classify all the devices that are tied for best affinity:
              ties = [ devid | (devid,aff) <- rest, aff == snd first ]
              -- Do tie breaking based on priority, larger is better:
              prioritized = reverse $ sortBy (\d1 d2 -> compare (prio (devMap#d1)) (prio (devMap#d2))) ties
              winner:_ = prioritized
              action = RunKernel winner (tskMap#tsk1)

              -- After we copy to the current device, subsequent
              -- accesses on this device have no affinity-penalty.
              newArrs = arrayMapMerge arrays $ M.fromList $
                         [ (dep, (fst(arrays#dep), S.singleton (mem$ devMap#winner)))
                         | dep <- S.toList (depends#tsk1) ]

              sched' = Sched{ freeDevs    = S.delete winner freeDevs 
                            , waiting     = S.union (S.fromList restReady) notready
                            , outstanding = S.insert tsk1 outstanding
                            , arrays      = newArrs
                            , workingDevs = workingDevs
                            }
          in
           -- trace ("Processing task "++show tsk1++",  bout to buffer action, "++show(w, prioritized)) $
           BufferAction action : loop sched' (Next [] kont) (action:acts)

    -- Here we process events from the runtime:
    loop Sched{..} (Next ls kont) actBuf = 
      let free' = S.union freeDevs (S.fromList [ d | DeviceFree d _ <- ls ]) 
          newArrs = arrayMapMerge arrays $ M.fromList $ 
                      [ (arrId, (sz,S.singleton (mem$ devMap#di)))
                      | DeviceFree di (Just (arrId,sz)) <- ls ]
          -- newArrs may have woken up waiting tasks:
          sched' = Sched{ freeDevs=free'
                        , arrays= newArrs
                        , outstanding = S.difference outstanding (M.keysSet newArrs)
                        , ..
                        }
      in Update sched' : loop sched' (Next [] kont) actBuf
    -----------------------------------------

    arrayMapMerge = M.unionWith combine 
      where
          combine (sz1,locs1) (sz2,locs2)
            | sz1 == sz2 = (sz1, S.union locs1 locs2)
            | otherwise = error$ "Internal error, array exists in two places with different sizes: "
                                 ++show(sz1,locs1)++" and "++ show(sz2,locs2)

    -- The affinity is defined as how much data I would need to move
    -- to run here, lower is better.  It would be straightforward to
    -- generalize this to use an estimate of the specific transfer
    -- cost rather than assuming a fixed transfer cost/byte.
    computeAffinity tskId (Device{mem}) arrays =
      let deps    = S.toList (depends # tskId)
          -- Which arrays are NOT residing in the current memory
          notHere = [ sz | d <- deps 
                         , let (sz,places) = (arrays#d)
                         , S.member mem places ]
      in sum notHere
    
theMachine :: EventStream
theMachine =
  -- The first event is that all devices become free, and then we go
  -- into a loop.
  Next [DeviceFree (did d) Nothing | d <- devices] (loop 0.0 [])
 where
   loop :: Time -> [(Time,(TaskID,Size),DevID)] -> [Action] -> EventStream
   loop t [] [] =
     trace ("Warning: no busy devices, but also no requests from the sched") $ 
     Next [] (loop t []) -- Maybe issue a warning.

   -- Move the clock forward, release a device.  TODO: could release
   -- multiple devices if they became free at the exact same time.
   loop _t ((t2,arr,dev1):devN) [] =
     trace (" [machine]: releasing device: "++show(dev1,arr)++", at time "++show t2) $
     Next [DeviceFree dev1 (Just arr)] (loop t2 devN)

   loop t busyDevs (RunKernel dev tsk : acts)
     | L.elem dev [ d | (_,_,d) <- busyDevs] =
       error$ "Invalid scheduler: requested to schedule task "++show tsk
               ++" on device "++show dev++", which is busy."
     | otherwise = 
       -- Schedule the task to run, at a completion time in the future:
       let (tm,sz) = runTask tsk dev in
       -- TODO: return the size.
       trace (" [machine]: scheduling device: "++show dev++", until time "++show tm) $
       loop t (sort ((tm,(tid tsk,sz),dev):busyDevs)) acts

--------------------------------------------------------------------------------

devices :: [Device]
devices = [cpu1,cpu2,gpu1,gpu2]

-- | A four-chip machine like delta, with the CPUs exposed as two backends.
cpu1 = Device "cpu1" CPU 1.0 
cpu2 = Device "cpu2" CPU 1.0
gpu1 = Device "gpu1" GPU1 2.1
gpu2 = Device "gpu2" GPU2 2.0

job1 = TaskGraph [t1,t2] (M.fromList [(tid t1, S.empty), (tid t2, S.singleton (tid t1))])
  where
    t1 = Task 101 
    t2 = Task 102 

-- You can confirm that these get launched together because there is no dependency:
job2 = TaskGraph [t1,t2] (M.fromList [(tid t1, S.empty), (tid t2, S.empty)])
  where
    t1 = Task 201 
    t2 = Task 202 


-- | Only when we run a task, do we find out how much time it took,
-- and how big of an output it produced:
-- Note: Time varies based on what device it runs on, but output size may not.
runTask :: Task -> DevID -> (Time,Size)
runTask (Task 101) "cpu1" = (3.3, 100)
runTask (Task 101) _      = (3.5, 100)
runTask (Task 102) "cpu1" = (4.4, 100)
runTask (Task 102) _      = (4.8, 100)

--------------------------------------------------------------------------------

(#) :: (Show a1, Show a, Ord a1) => Map a1 a -> a1 -> a
m # v = case M.lookup v m of
          Nothing -> error$ "Map lookup failed, key: "++show v++", in map:\n"++show m
          Just x  -> x

main :: IO ()
main = do
  putStrLn "Running scheduler:"
  let ls = scheduler M.empty [cpu1,cpu2,gpu1,gpu2] job1 theMachine
  mapM_ print (take 50 ls)  
  return ()
