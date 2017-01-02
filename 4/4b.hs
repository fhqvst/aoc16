import Data.Char
import qualified Data.List as L
import Data.Ord
import Data.Maybe
import Data.Foldable

parseRoom :: String -> (String, Int, String)
parseRoom room = (name, read id :: Int, checksum)
  where
    name = init $ takeWhile (not . isNumber) room
    id = reverse $ takeWhile isNumber $  dropWhile (not . isNumber) $ reverse room
    checksum = filter isAlpha $ snd $ break (== '[') room

frequencies :: Ord a => [a] -> [(a,Int)]
frequencies l = map (\x -> (head x, length x)) . L.group . L.sort $ l

encrypt :: String -> String
encrypt string = take 5 . map fst $ concatenated
  where
    freqs = frequencies . filter isAlpha $ string
    sorted = L.sortOn snd $ freqs
    grouped = L.groupBy (\x y -> (snd x) == snd y) sorted
    concatenated = concat . reverse $ grouped

isValidRoom :: (String, Int, String) -> Bool
isValidRoom (name, id, checksum) = checksum == encrypt name

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

alphabet = toList "abcdefghijklmnopqrstuvwxyz"
decryptName :: (String, Int, String) -> String
decryptName (name, id, _) = map decryptChar name
  where
    decryptChar '-' = ' '
    decryptChar x = alphabet !! ((id + (fromJust (L.elemIndex x alphabet))) `mod` length alphabet)

main = do
  rooms <- return . map parseRoom . lines =<< readFile "input"

  let validRooms = filter isValidRoom rooms
  let decryptedNames = map decryptName validRooms
  let zipped = zip decryptedNames $ map snd3 validRooms

  mapM putStrLn . map show . filter (\x -> not . null . L.intersect ["north","pole","object"] . words . fst $ x) $ zipped

  putStrLn "done"
