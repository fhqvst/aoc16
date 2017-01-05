import Data.List
import Data.Ord
main = putStrLn =<< (map (head . maximumBy (comparing length) . group . sort) . transpose . words) <$> readFile "input"
