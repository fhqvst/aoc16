parseShape :: String -> [Int]
parseShape s = map read $ words s

isTriangle :: [Int] -> Bool
isTriangle (x:y:z:zs) = and [
    x + y > z,
    x + z > y,
    y + z > x
  ]

main = do
  shapes <- (return . map parseShape . lines) =<< readFile "input"
  putStrLn $ show $ length $ filter isTriangle shapes
  putStrLn "done"
