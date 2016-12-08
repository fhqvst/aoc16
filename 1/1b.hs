import qualified System.IO.Strict as S
import qualified Data.Foldable as F
import qualified Data.Char as C
import Data.List as L


walk :: (Char,Int,Int) -> (Char, Int) -> (Char,Int,Int)
walk (facing,x,y) (dir,steps) = (newDir, x + stepsX, y + stepsY) where
  stepsX
    | (facing == 'N' && dir == 'R') || (facing == 'S' && dir == 'L') = steps
    | (facing == 'N' && dir == 'L') || (facing == 'S' && dir == 'R') = (-steps)
    | (facing == 'E') && dir == 'F' = steps
    | (facing == 'W') && dir == 'F' = (-steps)
    | otherwise = 0
  stepsY 
    | (facing == 'W' && dir == 'R') || (facing == 'E' && dir == 'L') = steps
    | (facing == 'W' && dir == 'L') || (facing == 'E' && dir == 'R') = (-steps)
    | (facing == 'N') && dir == 'F' = steps
    | (facing == 'S') && dir == 'F' = (-steps)
    | otherwise = 0
  newDir
    | dir == 'F' = facing
    | (facing == 'W' && dir == 'R') || (facing == 'E' && dir == 'L') = 'N'
    | (facing == 'N' && dir == 'R') || (facing == 'S' && dir == 'L') = 'E'
    | (facing == 'E' && dir == 'R') || (facing == 'W' && dir == 'L') = 'S'
    | (facing == 'N' && dir == 'L') || (facing == 'S' && dir == 'R') = 'W'

walkDirs from dirs = F.foldl' walk from dirs

answer (d,x,y) = abs x + (abs y)

positions :: (Char,Int,Int) -> [(Char, Int)] -> [(Char, Int, Int)]
positions from [] = []
positions from (d:ds) = [(walk from d)] ++ (positions (walk from d) ds)

-- Recursively walk and return the current position

main = do
  input <- S.readFile "input"

  let directions = map (\str -> (str !! 0, read (takeWhile C.isDigit $ drop 1 str) :: Int)) (words input)
  let oners = concatMap (\(d,s) -> if s > 1 then ([(d,1)] ++ replicate (s-1) ('F',1)) else [(d,1)]) directions  
  
  let visited = positions ('N',0,0) oners
  let filtered = L.nubBy (\(_,x1,y1) (_,x2,y2) -> ((x1 == x2) && (y1 == y2))) visited

  let final = visited \\ filtered

  let end = head final

  putStrLn $ "Answer: " ++ (show $ answer end)
