import Pipes (yield, lift, (~>))
import Pipes.Prelude (fold)
import System.Environment (getArgs)
import System.Posix.Files (fileSize, getSymbolicLinkStatus)
import System.Posix.Types (FileOffset)
import Data.List (sortBy)
import Util.StreamDirectory (getRecursiveContents)
import Util.HeapFunctions (boundedInsert)
import qualified Data.Heap as H

data FilePathPair a = Pair FilePath a
    deriving (Eq, Show)

instance Ord a => Ord (FilePathPair a) where
    compare (Pair _ x) (Pair _ y) = compare x y

topFilesBy :: Ord a => (FilePath -> IO a) -> Int -> FilePath -> IO (H.Heap H.MinPolicy (FilePathPair a))
topFilesBy f k = fold (boundedInsert k) H.empty id . getPairs where
    getPairs = getRecursiveContents ~> \file -> do
        size <- lift $ f file
        yield $ Pair file size

biggestFiles :: Int -> FilePath -> IO (H.Heap H.MinPolicy (FilePathPair FileOffset))
biggestFiles = topFilesBy getFileSize where
    getFileSize = fmap fileSize . getSymbolicLinkStatus

main :: IO ()
main = do
    [startDir, num] <- getArgs
    heap <- biggestFiles (read num) startDir
    mapM_ print . sortBy (flip compare) . H.toList $ heap
