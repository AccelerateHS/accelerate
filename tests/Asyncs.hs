
data Async a = MVar (a, Set MemID)

data Asyncs a where
  Asyncs :: AsyncsR (ArrRepr a)
         -> Asyncs a

data family AsyncsR :: *
data instance AsyncsR ()           = A_Unit
data instance AsyncsR (Array sh e) = A_Array (Async a)
data instance AsyncsR (a,b)        = A_Pair (AsyncsR a) (AsyncsR b)



async :: forall a. Arrays a => a -> IO (Asyncs a)
async a = Asyncs <$> go (arrays (undefined::a))
  where
    go :: ArraysR t -> t -> IO (AsyncsR t)
    go ArraysRunit ()            = return A_Unit
    go (ArraysRPair a1 a2) (a,b) = A_Pair <$> go a1 a <*> go a2 b
    go ArraysRarray a            = do {- copy, etc... -}
                                      return $ A_Array undefined

wait :: forall a. Arrays a => Asyncs a -> IO a
wait a = go (arrays (undefined::a)))
  where
    go :: ArraysR t -> AsyncsR t -> IO t
    go ArraysRunit            A_Unit        = return ()
    go (ArraysRpair ad1 ad2) (A_Pair a1 a2) = (,) <$> go ad1 a1 <*> go ad2 a2
    go (ArraysRarray _)      (A_Array a)    = do {- wait -}

executeMultiAcc
    :: Arrays a
    => DelayedOpenAcc a
    -> IO (Asyncs a)
executeMultiAcc = undefined

