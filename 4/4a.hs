import Data.Char
import qualified Data.List as L
import Data.Ord

parseRoom :: String -> (String, Int, String)
parseRoom room = (name, read id :: Int, checksum)
  where
    name = init $ takeWhile (not . isNumber) room
    id = reverse $ takeWhile isNumber $  dropWhile (not . isNumber) $ reverse room
    checksum = filter isAlpha $ snd $ break (== '[') room

frequencies :: Ord a => [a] -> [(a,Int)]
frequencies l = map (\x -> (head x, length x)) . L.group . L.sort $ l

encrypt :: String -> String
encrypt string = take 5 . map fst $ sorted
  where
    freqs = frequencies . filter isAlpha $ string
    sorted = L.sortOn snd $ freqs

isValidRoom :: (String, Int, String) -> Bool
isValidRoom (name, id, checksum) = checksum == encrypt name

getRoomName (name, _, _) = name
getRoomChecksum (_, _, checksum) = checksum

getRoomId :: (String, Int, String) -> Int
getRoomId (_, id, _) = id

main = do
  rooms <- return . map parseRoom . lines =<< readFile "input"

  let r = rooms !! 0

  putStrLn $ encrypt . getRoomName $ r
  putStrLn $ getRoomChecksum $ r
  putStrLn $ show . L.maximumBy (comparing snd) . init . L.sortOn snd. frequencies . filter isAlpha $ getRoomName $ r

  let validRooms = filter isValidRoom rooms
  putStrLn . show . sum . map getRoomId $ validRooms
  putStrLn "done"
