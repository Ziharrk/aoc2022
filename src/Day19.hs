module Day19 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Text.Parsec (Parsec, string, newline, many1, digit)
import Text.Parsec.Text (parseFromFile)

import Debug.Trace

data FactoryBlueprint = FactoryBlueprint Int (Map MaterialType Robot)

data Robot = Robot {
    robotType :: MaterialType,
    robotCosts :: Map MaterialType Int
  }

data MaterialType = Ore | Clay | Obsidian | Geode
  deriving (Eq, Ord, Show)

parseBlueprint :: Parsec Text () FactoryBlueprint
parseBlueprint = do
  _ <- string "Blueprint "
  n <- int
  _ <- string ": Each ore robot costs "
  numOre <- int
  _ <- string " ore. Each clay robot costs "
  numClay <- int
  _ <- string " ore. Each obsidian robot costs "
  num1Obsi <- int
  _ <- string " ore and "
  num2Obsi <- int
  _ <- string " clay. Each geode robot costs "
  num1Geode <- int
  _ <- string " ore and "
  num2Geode <- int
  _ <- string " obsidian."
  _ <- newline
  return $ FactoryBlueprint n $ Map.fromList [
    (Ore, Robot Ore $ Map.fromList [(Ore, negate numOre)]),
    (Clay, Robot Clay $ Map.fromList [(Ore, negate numClay)]),
    (Obsidian, Robot Obsidian $ Map.fromList [(Ore, negate num1Obsi), (Clay, negate num2Obsi)]),
    (Geode, Robot Geode $ Map.fromList [(Ore, negate num1Geode), (Obsidian, negate num2Geode)])
    ]

int :: Parsec Text () Int
int = read <$> many1 digit

evaluateBlueprint :: Int -> FactoryBlueprint -> Int
evaluateBlueprint t (FactoryBlueprint n blueprints) =
  n * go t Map.empty (Map.singleton Ore 1)
  where
    maxOreCost  = maximum $ map (negate . Map.findWithDefault 0 Ore . robotCosts) $ Map.elems blueprints
    maxClayCost = maximum $ map (negate . Map.findWithDefault 0 Clay . robotCosts) $ Map.elems blueprints
    maxObsiCost = maximum $ map (negate . Map.findWithDefault 0 Obsidian . robotCosts) $ Map.elems blueprints
    go 0 mats _ = Map.findWithDefault 0 Geode mats
    go time mats bots
      | sum (take (time-2) [Map.findWithDefault 0 Obsidian bots..]) +
        Map.findWithDefault 0 Obsidian mats
          < maxObsiCost                     = Map.findWithDefault 0 Geode mats + Map.findWithDefault 0 Geode bots * time
      | null possibilities                  = go (time - 1) (Map.unionWith (+) mats bots) bots
      -- Always build geode robots if possible
      | all (>= 0) geode = go (time - 1) (Map.unionWith (+) (Map.unionWith (+) mats (robotCosts $ blueprints Map.! Geode)) bots) (Map.insertWith (+) Geode 1 bots)
      -- If we have enough ore for anything * 1.25, do not do nothing
      | Map.findWithDefault 0 Ore mats >= floor ((fromIntegral maxOreCost :: Float) * 1.25) = foldr max 0 possibilities
      | otherwise = foldr max (go (time - 1) (Map.unionWith (+) mats bots) bots) possibilities
      where
        possibilities =
          [ go (time - 1) matsCollect bots'
          | typ <- [Ore, Clay, Obsidian, Geode]
          , let (b, matsConstruct) = optimizationConstraint Map.! typ
          , flip const (show typ ++ " " ++ show mats ++ " " ++ show b) b
          , all (>= 0) matsConstruct
          , let matsCollect = Map.unionWith (+) matsConstruct bots
          , let bots' = Map.insertWith (+) typ 1 bots
          ]
        optimizationConstraint = Map.fromList
          [ (Ore, (numOreWorkers < maxOreCost, ore))
          , (Clay, (numClayWorkers < maxClayCost, clay) )
          , (Obsidian, (numObsiWorkers < maxObsiCost, obsi))
          , (Geode, (True, geode))
          ]
        numOreWorkers = Map.findWithDefault 0 Ore bots
        numClayWorkers = Map.findWithDefault 0 Clay bots
        numObsiWorkers = Map.findWithDefault 0 Obsidian bots
        ore   = Map.unionWith (+) mats (robotCosts (blueprints Map.! Ore))
        clay  = Map.unionWith (+) mats (robotCosts (blueprints Map.! Clay))
        obsi  = Map.unionWith (+) mats (robotCosts (blueprints Map.! Obsidian))
        geode = Map.unionWith (+) mats (robotCosts (blueprints Map.! Geode))

day19 :: IO ()
day19 = do
  res <- parseFromFile (many1 parseBlueprint) "input/day19"
  case res of
    Left err -> print err
    Right blueprints -> do
      let evaluated = map (evaluateBlueprint 24) blueprints
      print evaluated
      putStr "Part 1: "
      print (sum evaluated)
      let evaluated2 = zipWith (\b n -> evaluateBlueprint 32 b `div` n) blueprints [1, 2, 3]
      print evaluated2
      putStr "Part 2: "
      print (product evaluated2)
