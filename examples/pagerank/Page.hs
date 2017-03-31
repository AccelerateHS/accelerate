{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Page (

  PageId, Rank, Link, MLinks(..),
  parsePage, parsePageId

) where

import Prelude                                   as P
import qualified Data.ByteString.Lazy.Char8      as BL
import qualified Data.Vector.Storable.Mutable    as UM
import Data.Word


-- | Unique identifier for a page.
type PageId
        = Word32


-- | Represents a link in the page raph.
type Link
        = (PageId, PageId)


-- | A single PageRank value.
type Rank
        = Float

-- | A mutable set of links
data MLinks
        = MLinks
            { ml_ix    :: Int
            , ml_size  :: Int
            , ml_from  :: UM.IOVector PageId
            , ml_to    :: UM.IOVector PageId
            , ml_sizes :: UM.IOVector Int
            }


-- | Parse just the PageId from a line in the links file.
parsePageId :: BL.ByteString -> Maybe PageId
parsePageId bs
        | Just (pid, _)         <- BL.readInt bs
        = Just $ fromIntegral pid

        | otherwise
        = Nothing


-- | Parse a whole line of the links file.
parsePage :: BL.ByteString -> MLinks -> IO (Maybe MLinks)
parsePage bs links
        | Just (pid, bs2)       <- BL.readInt bs
        , Just bs3              <- char ':' bs2
        = do
             links'             <- pageIds (fromIntegral pid) bs3 links
             return (Just links')

        | otherwise
        = return Nothing


-- | Parse a single character.
char   :: Char -> BL.ByteString -> Maybe BL.ByteString
char c bs
 | BL.null bs           = Nothing
 | BL.head bs == c      = Just (BL.tail bs)
 | otherwise            = Nothing
{-# INLINE char #-}


-- | Parse a vector of PageIds.
pageIds    :: PageId -> BL.ByteString -> MLinks -> IO (MLinks)
pageIds pid bs0 links0
 = go links0 0 bs0

 where  go links@(MLinks{..}) count bs
         | ml_ix >= ml_size
         = do   from'  <- UM.grow ml_from ml_size
                to'    <- UM.grow ml_to   ml_size
                go (MLinks ml_ix (2*ml_size) from' to' ml_sizes) count bs

         | BL.null bs
         = final

         | Just bs2     <- char ' ' bs
         = go links count bs2

         | Just (i, bs2) <- BL.readInt bs
         = do   UM.write ml_from ml_ix pid
                UM.write ml_to   ml_ix (fromIntegral i)
                go (MLinks (ml_ix+1) ml_size ml_from ml_to ml_sizes) (count+1) bs2

         | otherwise
         = final
         where
            final =
                do
                    UM.write ml_sizes (fromIntegral pid) count
                    return links

