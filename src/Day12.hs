module Day12 where

import Control.Monad (forM_, when)
import Data.IntMap as IntMap (IntMap, fromList, (!), insert, elems, toList, size, lookup)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Data.List.Extra ((!?))
import Data.Maybe (mapMaybe)

data Graph = Graph { nodes :: IntMap Node, edges :: IntMap [Int] }

data Node = Node { identifier :: Int
                 , isGoal :: Bool
                 , isStart :: Bool
                 , heigth :: Int }

mkGraph :: [String] -> IO Graph
mkGraph input = do
  counterRef <- newIORef 0
  let sizeX = length input
      sizeY = length $ head input
      mkNode goal start heigth = do
        ident <- readIORef counterRef
        writeIORef counterRef (ident + 1)
        return (Node ident goal start heigth)
  let mkAllNodes [] = return []
      mkAllNodes (x:xs) = (:) <$> mkNodes x <*> mkAllNodes xs
      mkNodes [] = return []
      mkNodes (c:cs) = do
        let heigth = if c == 'E' then 25 else if c == 'S' then 0 else fromEnum c - fromEnum 'a'
        (:) <$> mkNode (c == 'E') (c == 'S') heigth <*> mkNodes cs
  curNodes <- mkAllNodes input

  let createEdges 0 0 = []
      createEdges 0 1 = []
      createEdges 0 y = createEdges sizeX (y - 1)
      createEdges x y =
        let Node ident _ _ heigth = curNodes !! (x-1) !! (y-1)
            edges = [ ident' | (x', y') <- [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
                             , Just tmp <- [curNodes !? (x' - 1)]
                             , Just (Node ident' _ _ heigth') <- [tmp !? (y' - 1)]
                             , heigth' <= heigth + 1]
        in (ident, edges) : createEdges (x - 1) y
  let curEdges = createEdges sizeX sizeY
  return (Graph (fromList $ map (\n@(Node i _ _ _) -> (i,n)) $ concat curNodes) (fromList curEdges))

shortestPaths :: Node -> IntMap [Int] -> IntMap Node -> IO (IntMap Int)
shortestPaths (Node ident _ _ _) edges nodes = do
  distanceRef <- newIORef (fromList [(ident, 0)])
  let allEdges = concatMap (\(n, m) -> map (n,) m) (toList edges)
  forM_ [1.. (size nodes - 1)] $ \iter -> do
    putStr ("\r" ++ show iter ++ " of " ++ show (size nodes - 1))
    forM_ allEdges $ \(v, u) -> do
      uDistM <- IntMap.lookup u <$> readIORef distanceRef
      vDistM <- IntMap.lookup v <$> readIORef distanceRef
      case (uDistM, vDistM) of
        (Nothing, _) -> return ()
        (Just uDist, Nothing) -> modifyIORef distanceRef (insert v (uDist + 1))
        (Just uDist, Just vDist) -> when (uDist + 1 < vDist) $ do
          modifyIORef distanceRef (insert v (uDist + 1))
  putStr "\n"
  readIORef distanceRef

day12 :: IO ()
day12 = do
  input <- lines <$> readFile "input/day12"
  Graph nodes edges <- mkGraph input
  putStrLn "Calculating shortest paths..."
  let startNode = head $ filter isStart (elems nodes)
  let endNode = head $ filter isGoal (elems nodes)
  distancesMap <- shortestPaths endNode edges nodes
  putStr "Part 1: "
  print (distancesMap ! identifier startNode)
  putStr "Part 2: "
  let startNodes = filter ((==0). heigth) (elems nodes)
  let distances = mapMaybe ((`IntMap.lookup` distancesMap) . identifier) startNodes
  print (minimum distances)
