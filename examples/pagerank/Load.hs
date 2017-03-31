{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternGuards   #-}
module Load where

import Page
import Progress

import qualified Data.ByteString.Lazy.Char8      as BL
import qualified Data.Vector.Storable.Mutable    as VM
import qualified Data.Vector.Storable            as V


-- | Load the whole page graph into memory.
loadPages
        :: FilePath             -- ^ Pages file.
        -> PageId               -- ^ Maximum page number
        -> IO ( V.Vector PageId -- ^ From vector
              , V.Vector PageId -- ^ To vector
              , V.Vector Int    -- ^ Degrees (number of outgoing links) for each page
              )

loadPages filePath maxPageId
 = do
        -- Lazilly read the pages files.
        bs              <- BL.readFile filePath

        -- Create an initial vector to hold the pages.
        -- We'll grow this as we read more pages.
        let bufSize     = 100000
        from            <- VM.new bufSize
        to              <- VM.new bufSize
        sizes           <- VM.new (fromIntegral maxPageId +1)
        go (MLinks 0 bufSize from to sizes) 0 (BL.lines bs)

 where  go links@(MLinks{..}) !ixLine ls
         -- We've read all the lines.
         -- Slice out the pages we read from the buffer.
         | []       <- ls
         = do   printPosition True "  lines read : " 10000 ixLine
                from'  <- V.freeze (VM.slice 0 ml_ix ml_from)
                to'    <- V.freeze (VM.slice 0 ml_ix ml_to)
                sizes' <- V.freeze ml_sizes

                return (from', to', sizes')

         -- Handle a new line from the file.
         | l : rest <- ls
         = do   -- Print how far along we are.
                printPosition False "  lines read : " 10000 ixLine

                -- Parse the page and add it to the buffer.
                Just links' <- parsePage l links
                go links' (ixLine+1) rest

         | otherwise
         = error "PageRank.loadPages: unexpected error"

