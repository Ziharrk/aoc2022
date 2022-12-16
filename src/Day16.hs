module Day16 where

import Data.Foldable (foldl')
import Data.List (intercalate, nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.IntMap qualified as Map
import System.Directory (createDirectoryIfMissing)
import Text.Parsec (digit, string, option, many1, Parsec, optional, sepBy1)
import Text.Parsec.Char (newline, space, anyChar, char)
import Text.Parsec.Text (parseFromFile)

data SensorData = MkData Int Int [Int]

type Parser = Parsec Text ()

parseData :: Parser SensorData
parseData = MkData <$>
  (string "Valve " *> valve) <*>
  (string " has flow rate="  *> int) <*>
  (string "; tunnel" *> optional (char 's') *>
    string " lead" *> optional (char 's') *>
    string " to valve" *> optional (char 's') *>
    space *> valve `sepBy1` (string ", "))
  where
    valveCodeToInt a b = fromEnum a - fromEnum 'A' +
      26 * (fromEnum b - fromEnum 'A')
    valve = valveCodeToInt <$> anyChar <*> anyChar
    int = (. read) <$> option id neg <*> many1 digit
    neg = negate <$ string "-"

mkDecisionTree :: Int -> Int -> Int -> [SensorData] -> Int
mkDecisionTree num guess rounds dat = goValve visMap guess rounds 0 (replicate num (Nothing, dataMap Map.! 0))
  where
    visMap = Map.map (const ()) $ Map.filter (\(MkData _ f _) -> f == 0) dataMap
    dataMap = Map.fromList (map (\d@(MkData v _ _) -> (v, d)) dat)
    goValve _   best 0 !acc _      = max best acc
    goValve vis best n !acc actors
      | Map.size vis == Map.size dataMap
      = acc
      | potential n vis dataMap actors + acc < best
      = best
      | otherwise
      = handle (n-1) best acc vis actors []

    handle n best !acc vis [] locs = goValve vis best n acc locs
    handle n best !acc vis ((prev, d@(MkData i f childs)):rest) locs =
      foldl' doNext best $
      case Map.insertLookupWithKey (\_ _ _ -> ()) i () vis of
        (Just (), _) -> [(Just i, dataMap Map.! i', 0, vis) | i' <- childs, Just i' /= prev]
        (_   , vis') -> (Nothing, d, f * n, vis') :
                          [(Just i, dataMap Map.! i', 0, vis) | i' <- childs, Just i' /= prev]

      where doNext best' (prev', d', v, vis') = handle n best' (acc + v) vis' rest ((prev', d') : locs)

potential :: Int -> Map.IntMap () -> Map.IntMap SensorData -> [(Maybe Int, SensorData)] -> Int
potential rnd visMap dataMap actors =
  sum $ Map.map (\(MkData i f _) -> f * max 0 (rnd - distanceToAll i actors - 1)) remaining
  where
    remaining = foldr (Map.delete) dataMap (Map.keys visMap)
    distanceToAll i = minimum . map (distanceTo' i)
    distanceTo' i (_, MkData j _ _) = distanceTo i [(j, 0)] Map.empty
    distanceTo _ [] _ = error "unreachable"
    distanceTo i ((j, n):js) visited
      | i == j = n
      | otherwise = case Map.lookup j visited of
          Just () -> distanceTo i js visited
          Nothing -> distanceTo i (js ++ childs) (Map.insert j () visited)
            where
              MkData _ _ cs = dataMap Map.! j
              childs = map (, n+1) cs

day16 :: IO ()
day16 = do
  res <- parseFromFile (many1 (parseData <* newline)) "input/day16"
  case res of
    Left err -> print err
    Right dat -> do
      createDirectoryIfMissing False "output"
      writeFile "output/day16.dot" (dotify dat)
      putStrLn "Wrote DOT file to 'output/day16.dot'."
      putStr "Part 1: "
      -- using a guess to speed up the process (only really used for part 2)
      print (mkDecisionTree 1 1500 30 dat)
      putStrLn "Caution: Not that memory intensive, but extremely slow!"
      putStr "Part 2: "
      print (mkDecisionTree 2 2200 26 dat)

dotify :: [SensorData] -> String
dotify dat = "graph G {" ++ intercalate ";\n" (untangled ++ shapes) ++ ";\n}\n"
  where
    untangled = map (\(i, j) -> show i ++ " -- " ++ show j) $ nub $ concatMap dataLines dat
    dataLines (MkData i _ childs) = map (\c -> (min i c, max i c)) childs
    shapes = mapMaybe shape dat
    shape (MkData i f _) = if f == 0 then Just (show i ++ " [shape=Mdiamond]") else Nothing
