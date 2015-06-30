{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Page
        ( PageId
        , Rank
        , Link
        , MLinks(..)
        , parsePage
        , parsePageId)
where
import Prelude                                   as P
import qualified Data.ByteString.Lazy.Char8      as BL
import qualified Data.Vector.Storable            as U
import qualified Data.Vector.Storable.Mutable    as UM
import Control.Monad.ST
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
data MLinks = MLinks { ix    :: Int
                     , size  :: Int
                     , from  :: (UM.IOVector PageId)
                     , to    :: (UM.IOVector PageId)
                     , sizes :: UM.IOVector Int }


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
pageIds pid bs0 links
 = go links 0 bs0

 where  go links@(MLinks{..}) count bs
         | ix >= size
         = do   from'  <- UM.grow from size
                to'    <- UM.grow to   size
                go (MLinks ix (2*size) from' to' sizes) count bs

         | BL.null bs
         = final

         | Just bs2     <- char ' ' bs
         = go links count bs2

         | Just (i, bs2) <- BL.readInt bs
         = do   UM.write from ix pid
                UM.write to ix (fromIntegral i)
                go (MLinks (ix+1) size from to sizes) (count+1) bs2

         | otherwise
         = final
         where
            final =
                do
                    UM.write sizes (fromIntegral pid) count
                    return links
