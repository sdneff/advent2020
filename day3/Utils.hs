module Utils where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

readTrees :: FilePath -> IO [T.Text]
readTrees path = fmap T.lines (TIO.readFile "trees.txt")

isTree :: (T.Text, Int) -> Bool
isTree (s,offset) = T.index s (offset `mod` (T.length s)) == '#'

getOffsets :: Int -> [Int]
getOffsets step = map (* step) [0..]

skip :: Int -> [a] -> [a]
skip n (x:xs) = x : (skip n (drop (n-1) xs))
skip n [] = []
