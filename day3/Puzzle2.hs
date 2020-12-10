import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Utils

treeCountXY :: [T.Text] -> (Int, Int) -> Int
treeCountXY rows (offsetX, offsetY) = length (filter isTree (zip (skip offsetY rows) (getOffsets offsetX)))

main = do
    rows <- readTrees "trees.txt"
    let vectors = [(1,1), (3,1), (5,1), (7,1), (1,2)]
    let trees = product (map (treeCountXY rows) vectors)
    putStrLn ("tree product: " ++ show trees)
