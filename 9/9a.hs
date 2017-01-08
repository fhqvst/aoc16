import Data.List
import Text.Regex
import Data.Char

parseMarker :: String -> (Int,Int,String)
parseMarker s = case matchRegexAll regex s of
  Nothing -> (0,0,"")
  Just (_,match,after,_) -> (fst (parseMarker' match), snd (parseMarker' match), after)
  where
    regex = mkRegex "\\([0-9]+x[0-9]+\\)"

parseMarker' :: String -> (Int,Int)
parseMarker' s = ((read chars)::Int, (read (drop 1 reps))::Int)
  where
    (chars, reps) = break (=='x') . init . tail $ s

decompress :: String -> String
decompress "" = ""
decompress s = (concat (replicate reps comp)) ++ (decompress remain)
  where
    (chars, reps, xs) = parseMarker s
    (comp, remain) = splitAt chars xs

main = do
  input <- readFile "input"
  putStrLn . show . length . decompress $ input
  putStrLn "done"
