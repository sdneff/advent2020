import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

import Utils

treeCountX :: [T.Text] -> Int -> Int
treeCountX rows offset = length (filter isTree (zip rows (getOffsets offset)))

main :: IO ()
main = do
    rows <- readTrees "trees.txt"
    putStrLn ("tree count: " ++ show (treeCountX rows 3))
