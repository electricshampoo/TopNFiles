import Pipes (yield, lift, (~>))
import Pipes.Prelude (fold)
import System.Environment (getArgs)
import System.Posix.Files (fileSize, getSymbolicLinkStatus)
import System.Posix.Types (FileOffset)
import Data.Foldable (toList)
import Util.StreamDirectory (getRecursiveContents)
import Util.HeapFunctions (boundedInsert)
import qualified Data.Heap as H

topFilesBy :: Ord a => (FilePath -> IO a) -> Int -> FilePath -> IO (H.Heap (H.Entry a FilePath))
topFilesBy f k = fold (boundedInsert k) H.empty id . getPairs where
    getPairs = getRecursiveContents ~> \file -> do
        val <- lift $ f file
        yield $ H.Entry val file

biggestFiles :: Int -> FilePath -> IO (H.Heap (H.Entry FileOffset FilePath))
biggestFiles = topFilesBy getFileSize where
    getFileSize = fmap fileSize . getSymbolicLinkStatus

main :: IO ()
main = do
    [startDir, num] <- getArgs
    heap <- biggestFiles (read num) startDir
    mapM_ print . reverse . toList $ heap
