import Data.List
import Data.List.Split
import Text.Regex

hypers :: String -> [String]
hypers ip = case matchRegexAll regex ip of
  Nothing -> []
  Just (before, match, after, _) -> [noBrackets match] ++ (hypers after)
  where
    regex = mkRegex "\\[[A-Za-z]*\\]"
    noBrackets = init . tail

unhypers :: String -> [String]
unhypers ip = case matchRegexAll regex ip of
  Nothing -> [ip]
  Just (before, match, after, _) -> [before] ++ (unhypers after)
  where
    regex = mkRegex "\\[[A-Za-z]*\\]"

subseqs :: Int -> String -> [String]
subseqs n s | n > length s = []
subseqs n s = [take n s] ++ (subseqs n $ drop 1 s)

validateAbba :: String -> Bool
validateAbba s = or $ map isAbba strings
  where
    strings = map (splitAt 2) . subseqs 4 $ s 
    isAbba (xs,ys) = (xs == reverse ys) && (xs !! 0 /= xs !!1)

validateTLS :: String -> Bool
validateTLS s = and (map (not . validateAbba) h) && or (map validateAbba u)
  where
    h = hypers s
    u = unhypers s

main = do
  input <- lines <$> readFile "input"
  let valids = filter validateTLS input
  putStrLn $ show $ length valids
  putStrLn "done"
