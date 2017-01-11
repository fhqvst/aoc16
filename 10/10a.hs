import Data.List

data Robot = Microbot Int Chip Chip
  deriving (Eq, Show)

data Chip = NilChip | Chip Int
  deriving (Eq, Show, Ord)

data Dest = NilDest | Robot Int | Output Int
  deriving (Eq, Show)

type Command = (Int, Dest, Dest)


-- Load a chip onto a bot
loadChip :: Robot -> Chip -> Robot
loadChip (Microbot id NilChip NilChip) c = Microbot id c NilChip
loadChip (Microbot id c1 NilChip) c2 = Microbot id c1 c2
loadChip r c = error "loadChip: bot capacity overflow"

-- Search a list for a bot
findBotById :: Int -> [(Int, Robot)] -> (Int, Robot)
findBotById id bots = case lookup id bots of
  Just b -> (id, b)
  Nothing -> error "findBotById: bot not found"

-- Initialize robot with starting values
initRobots :: [(Int, Robot)] -> String -> [(Int, Robot)]
initRobots bots s = (id, loadChip bot (Chip val)) : bots
  where
    w = words s
    val = read $ w !! 1
    id = read $ w !! 5
    bot = case lookup id bots of
      Just (Microbot id (Chip c) NilChip) -> Microbot id (Chip c) NilChip
      Nothing -> Microbot id NilChip NilChip
      otherwise -> error "initRobots: bot capacity overflow"

-- Parse a command into a triple (Bot id, Low destination, High destination)
parseCommand :: String -> Command
parseCommand s = (id, d1, d2)
  where
    w = words s
    parseDest d i = if d == "bot" then Robot i else Output i
    id = read $ w !! 1
    d1 = parseDest (w !! 5) . read $ w !! 6
    d2 = parseDest (w !! 10) . read $ w !! 11

-- Checks if a bot is fully loaded
isFullyLoaded :: Robot -> Bool
isFullyLoaded (Microbot _ (Chip c1) (Chip c2)) = True
isFullyLoaded (Microbot _ _ _) = False

-- Run a command
runCommand :: [(Int, Robot)] -> Command -> [(Int, Robot)]
runCommand robots (id, d1, d2) = if isFullyLoaded bot then deliverChip bot (id, d1, d2) robots else robots
  where
    (_, bot) = case lookup id robots of
      Just b -> findBotById id robots
      Nothing -> (id, Microbot id NilChip NilChip) -- If bot is non-existent, just move on

-- Deliver chips from a bot to other bots
deliverChip :: Robot -> Command -> [(Int, Robot)] -> [(Int, Robot)]
deliverChip (Microbot _ NilChip NilChip) _ _ = error "deliverChip: bot not fully loaded"
deliverChip (Microbot _ c1 c2) (id, d1, d2) robots = addedBots
  where
    low = min c1 c2
    high = max c1 c2
    (lId, Microbot _ l1 _) = findBotById d1 robots
    (hId, Microbot _ h1 _) = findBotById d2 robots
    deletedBots = delete (id, Microbot id c1 c2) . delete (lId, Microbot lId l1 NilChip) . delete (hId, Microbot hId h1 NilChip) $ robots
    addedBots = (id, Microbot id NilChip NilChip):(d1, Microbot d1 l1 low):(d2, Microbot d2 h1 high):deletedBots

main = do
  input <- lines <$> readFile "input"
  
  let initValues = filter ((=='v') . head) input
  let initialized = foldl initRobots [] $ initValues

  let commands = map parseCommand $ filter ((=='b') . head) input
  let launched = foldl runCommand initialized commands

  mapM (putStrLn . show) initialized 
  mapM (putStrLn . show) commands

  putStrLn "done"
