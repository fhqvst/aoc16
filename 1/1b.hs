import qualified System.IO.Strict as S
import qualified Data.Foldable as F
import qualified Data.Char as C
walk :: (Char,Integer,Integer) -> (Char, Integer) -> (Char,Integer,Integer)
walk (facing,x,y) (dir,steps) = (newDir, x + stepsX, y + stepsY) where
  stepsX
    | (facing == 'N' && dir == 'R') || (facing == 'S' && dir == 'L') = steps
    | (facing == 'N' && dir == 'L') || (facing == 'S' && dir == 'R') = (-steps)
    | otherwise = 0
  stepsY 
    | (facing == 'W' && dir == 'R') || (facing == 'E' && dir == 'L') = steps
    | (facing == 'W' && dir == 'L') || (facing == 'E' && dir == 'R') = (-steps)
    | otherwise = 0
  newDir
    | (facing == 'W' && dir == 'R') || (facing == 'E' && dir == 'L') = 'N'
    | (facing == 'N' && dir == 'R') || (facing == 'S' && dir == 'L') = 'E'
    | (facing == 'E' && dir == 'R') || (facing == 'W' && dir == 'L') = 'S'
    | (facing == 'N' && dir == 'L') || (facing == 'S' && dir == 'R') = 'W'

answer (d,x,y) = abs x + (abs y)

main = do
  input <- S.readFile "input"

  let directions = map (\str -> (str !! 0, read (takeWhile C.isDigit $ drop 1 str) :: Integer)) (words input)
  let end = F.foldl' walk ('N',0,0) $ directions
  putStrLn $ "Answer: " ++ (show $ answer end)
