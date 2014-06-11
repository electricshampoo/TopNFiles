module Util.HeapFunctions
(boundedInsert
) where

import qualified Data.Heap as H

boundedInsert :: Ord a => Int -> H.Heap H.MinPolicy a -> a -> H.Heap H.MinPolicy a
boundedInsert 0 _ _ = H.empty
boundedInsert k heap val
    | k < 0 = error "The bounding size should not be negative."
    | k < H.size heap = boundedInsert k heap' val
    | k == H.size heap = if val < minItem then heap else H.insert val heap'
    | otherwise = H.insert val heap
    where (minItem, heap') = unsafeFromMaybe $ H.view heap
          unsafeFromMaybe (Just x) = x 
          unsafeFromMaybe _ = error "This cannot happen."
