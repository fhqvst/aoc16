import Data.List
import Data.Ord
main = putStrLn =<< (map (head . minimumBy (comparing length) . group . sort) . transpose . words) <$> readFile "input"
