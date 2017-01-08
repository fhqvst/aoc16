import Parsing
import Control.Monad
import Data.Maybe
import Data.List

-- Screen size
sw = 50
sh = 6
screen = replicate sh . replicate sw $ (0::Int)

-- Parse a string
string :: String -> Parser String
string ""    = return ""
string (c:s) = do c' <- char c
                  s' <- string s
                  return (c':s')

-- Parse a number
number :: Parser Int
number = do s <- oneOrMore digit
            return (read s::Int)

-- An operation on a screen
data Operation = Op ([[Int]] -> [[Int]])

-- Parsers for an operation
operation, rotation, rectangle :: Parser Operation 
operation = rotation <|> rectangle

-- Parse rotation
rotation = do
  string "rotate "
  dir <- (string "row") <|> (string "column")
  (string " y=") <|> (string " x=")
  n <- number
  string " by "
  k <- number
  return $ Op (rotate dir n k)

-- Parse rectangle
rectangle = do
  string "rect "
  w <- number
  char 'x'
  h <- number
  return $ Op (rectangulate w h)

-- Shift a list
shiftRight :: Int -> [a] -> [a]
shiftRight n xs = take (length xs) . drop (n * (length xs - 1)) . cycle $ xs

-- Perform a rotation
rotate :: String -> Int -> Int -> [[Int]] -> [[Int]]
rotate dir n k s = case dir of
  "row" -> perform shiftRight s
  "column" -> transpose . perform shiftRight . transpose $ s
  where
    perform sh xs = (take n xs) ++ [(sh k (xs !! n))] ++ (drop (n+1) xs)

-- Draw a rectangle
rectangulate :: Int -> Int -> [[Int]] -> [[Int]]
-- rectangulate w h s = (replicate h ((replicate w (1::Int)) ++ (replicate (sw - w) (0::Int)))) ++ drop h s
rectangulate w h s = map (\row -> replicate w (1::Int) ++ (drop w row)) (take h s) ++ drop h s

-- Parse an operation
parseOperation :: String -> Operation
parseOperation s = case parse operation s of
  Just (op, "") -> op
  Nothing -> error "parseOperation failed"

-- Perform an operation such as rotating or drawing a rectangle
operate :: Operation -> [[Int]] -> [[Int]]
operate (Op f) = f

-- Print the screen
printScreen :: [[Int]] -> IO ()
printScreen s = putStrLn $ unlines $ map (map (\c -> if c == 1 then '#' else '.')) s

main = do
  operations <- reverse . map parseOperation . lines <$> readFile "input"
  let screen2 = foldr operate screen operations
  printScreen screen2
  putStrLn . show . sum . concat $ screen2

  putStrLn "done"
