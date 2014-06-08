import Pipes (yield, lift, Producer, (~>))
import Pipes.Prelude (fold)
import System.Environment (getArgs)
import System.Posix.Files (fileSize, getSymbolicLinkStatus)
import System.Posix.Types (FileOffset)
import Data.List (sortBy)
import Util.StreamDirectory (getRecursiveContents)
import qualified Data.Heap as H

getFileSize :: FilePath -> IO FileOffset
getFileSize path = do
    stat <- getSymbolicLinkStatus path
    return $ fileSize stat

data FileSizePair = Pair FileOffset FilePath
    deriving (Eq, Show)

instance Ord FileSizePair where
    compare (Pair x _) (Pair y _) = compare x y

getFileSizes :: FilePath -> Producer FileSizePair IO ()
getFileSizes = getRecursiveContents ~> \file -> do
    size <- lift $ getFileSize file
    yield $ Pair size file

-- keeps heap at most size k and keeps the largest k so far
-- SELF INVENTED AHHHHHHHHHHHHHHHHH
-- using min heap to track the largest elements #yoloswag
beastInsert :: Ord a => Int -> H.Heap H.MinPolicy a -> a -> H.Heap H.MinPolicy a
beastInsert 0 _ _ = H.empty
beastInsert k heap val
    | k < 0 = error "The bounding size should not be negative."
    | k < H.size heap = beastInsert k heap' val
    | k == H.size heap = if val < minItem then heap else H.insert val heap'
    | otherwise = H.insert val heap
    where (minItem, heap') = unsafeFromMaybe $ H.view heap
          unsafeFromMaybe (Just x) = x 
          unsafeFromMaybe _ = error "This cannot happen."

biggestFiles :: Int -> FilePath -> IO (H.Heap H.MinPolicy FileSizePair)
biggestFiles k = fold (beastInsert k) H.empty id . getFileSizes 

main :: IO ()
main = do
    [startDir, num] <- getArgs
    heap <- biggestFiles (read num) startDir
    print . sortBy (flip compare) . H.toList $ heap
