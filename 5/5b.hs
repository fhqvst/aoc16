import Data.Digest.Pure.MD5 (md5)
import Data.Char
import Data.List
import qualified Data.ByteString.Lazy.Char8 as C

hash :: (String, Int) -> String
hash (s, i) = show . md5 . C.pack $ s ++ (show i)

isValidHash :: String -> Int -> Bool
isValidHash id i = ("00000" == take 5 h) && 8 > digitToInt (h !! 5)
  where h = hash (id,i)

indices :: String -> [Int]
indices id = take 20 [ x | x <- [0..], isValidHash id x ]

password :: String -> String -> String
password x y = if (x !! position) == '_' then a ++ [character] ++ (drop 1 b) else x
  where
    position = digitToInt $ y !! 5
    character = y !! 6
    (a, b) = splitAt position x

main = do
  let id = "abbhdwsy"
  putStrLn "Brute forcing indices..."
  let hashes = map (\x -> hash (id, x)) $ indices id
  let p = foldl password "________" hashes
  putStrLn $ "Password: " ++ p
  putStrLn "Done"
