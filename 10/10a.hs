data Robot = Microbot Chip Chip Dest Dest
  deriving (Eq, Show)

data Chip = NilChip | Chip Int
  deriving (Eq, Show)

data Dest = NilDest | Robot Int | Output Int
  deriving (Eq, Show)

type Command = (Int, Dest, Dest)

loadChip :: Robot -> Chip -> Robot
loadChip (Microbot NilChip NilChip d1 d2) c = Microbot c NilChip d1 d2
loadChip (Microbot c1 NilChip d1 d2) c2 = Microbot c1 c2 d1 d2
loadChip r c = error "bot capacity overflow"

initRobots :: [(Int, Robot)] -> String -> [(Int, Robot)]
initRobots bots s = (id, loadChip bot (Chip val)) : bots
  where
    w = words s
    val = read $ w !! 1
    id = read $ w !! 5
    bot = case lookup id bots of
      Just (Microbot (Chip c) NilChip NilDest NilDest) -> Microbot (Chip c) NilChip NilDest NilDest
      Nothing -> Microbot NilChip NilChip NilDest NilDest
      otherwise -> error "bot capacity overflow"

parseCommand :: String -> Command
parseCommand s = (id, d1, d2)
  where
    w = words s
    parseDest d i = if d == "bot" then Robot i else Output i
    id = read $ w !! 1
    d1 = parseDest (w !! 5) . read $ w !! 6
    d2 = parseDest (w !! 10) . read $ w !! 11

runCommand :: [(Int, Robot)] -> Command -> (Int, Robot)
runCommand bots (id, d1, d2) = (id, bot)
  where
    bot = case lookup id bots of
      Just (Microbot c1 c2 NilDest NilDest) -> Microbot c1 c2 d1 d2
      Nothing -> Microbot NilChip NilChip d1 d2
      otherwise -> error "bot already commanded"

isLoaded :: (Int, Robot) -> Bool
isLoaded (_, (Microbot NilChip NilChip _ _)) = False
isLoaded bot = True

launchRobot :: [(Int, Robot)] -> (Int, Robot) -> [(Int, Robot)]
launchRobot bots (id, (Microbot c1 c2 d1 d2)) = 
  where
    bot1 = case lookup id bots of
      Just (Microbot c1 c2 d1 d2) = Microbot c1 c2 d1 d2
      Nothing = error "bot not found"
      

main = do
  input <- lines <$> readFile "input"
  
  let initValues = filter ((=='v') . head) input
  let commands = map parseCommand $ filter ((=='b') . head) input

  let initialized = foldl initRobots [] initValues
  let aimed = filter isLoaded . map (runCommand initialized) $ commands

  let launched = foldl launchRobot aimed aimed

  mapM (putStrLn . show) aimed

  putStrLn "done"
