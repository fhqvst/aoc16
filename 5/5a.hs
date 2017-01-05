import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as C

hash :: (String, Int) -> String
hash (s, i) = show . md5 . C.pack $ s ++ (show i)

isValidHash :: String -> Int -> Bool
isValidHash id i = "00000" == take 5 (hash (id, i))

indices :: String -> [Int]
indices id = take 8 [ x | x <- [0..], isValidHash id x ]

main = do
  let id = "abbhdwsy"
  putStrLn "Brute forcing indices..."
  putStrLn $ "Password: " ++ (map (\x -> hash (id, x) !! 5) $ indices id)
