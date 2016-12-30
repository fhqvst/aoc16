import Control.Monad
import qualified Data.Char as C
import Data.List
import Data.Maybe

keypad = [[1,2,3],[4,5,6],[7,8,9]]

getMove :: Char -> Int
getMove c = case c of 'U' -> (-3)
                      'R' -> 1
                      'D' -> 3
                      'L' -> (-1)
                      _ -> 0

-- There are 2 criteria for the move to be valid:
-- 1: Only numbers between 1 and 9 (inclusive) are allowed
-- 2: Move only in one direction
doMove :: Int -> Char -> Int
doMove n c = if v1 && v2 then nextDigit else n
  where
    currentRow = findIndex (elem n) keypad
    currentCol = elemIndex n (keypad !! (fromJust currentRow))
    isVert = elem c "UD"
    nextDigit = n + getMove c
    v1 = (nextDigit >= 1) && nextDigit <= 9
    v2 = if isVert 
      then currentCol == elemIndex nextDigit (keypad !! (fromJust (findIndex (elem nextDigit) keypad)))
      else currentRow == findIndex (elem nextDigit) keypad

getDigits :: [[Char]] -> [Int]
getDigits [] = []
getDigits (x:xs) = [getDigit (x:xs)] ++ getDigits xs

-- Calculate a digit based on the next one
getDigit :: [[Char]] -> Int
getDigit (x:[]) = foldl doMove 5 x
getDigit (x:xs) = foldl doMove (getDigit xs) x

main :: IO()
main = do

  -- Get the operations for each digit (reversed - last digit first)
  operations <- (return . reverse . lines) =<< readFile "input"
  let digits = reverse $ getDigits operations
  print digits
  putStrLn "Done"
