{-# LANGUAGE TypeFamilies #-}
module Day7 where

import Data.Functor.Identity (Identity)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as Map (lookup, insert, empty, mapWithKey)
import Data.Maybe (listToMaybe)
import Control.Arrow (first, second)
import Control.Monad.State (when, liftIO, gets, modify, StateT, evalStateT, runState)

type family TreePurity a b where
  TreePurity IORef b = IORef b
  TreePurity Identity b = b

data FileTree io = FileTree
  { dirs  :: TreePurity io (Map FilePath (FileTree io))
  , files :: TreePurity io [(String, Int)] }

type FileTreeIO = FileTree IORef
type FileTreePure = FileTree Identity

purifyTree :: FileTreeIO -> IO FileTreePure
purifyTree (FileTree dirs files) = do
  dirs' <- readIORef dirs
  files' <- readIORef files
  FileTree <$> mapM purifyTree dirs' <*> pure files'

type StateIO m = StateT m IO

getCmd :: StateIO [String] (Maybe String)
getCmd = do
  res <- gets listToMaybe
  modify (drop 1)
  return res

toFileTree :: [String] -> IO FileTreeIO
toFileTree input = do
  r1 <- newIORef Map.empty
  r2 <- newIORef []
  evalStateT (go (FileTree r1 r2)) (drop 1 input)
  where
    go tree = do
      mbx <- getCmd
      case mbx of
        Nothing -> return tree
        Just x  -> case words x of
          ["$", "cd", dir] -> changeDir dir
          ["$", "ls"]      -> go tree
          ["dir", dir]     -> addDir dir
          [bytes, file]    -> addFile (read bytes) file
          _                -> error "invalid input"
      where
        changeDir ".." = return tree -- no "go tree" here!
        changeDir dir  = do
          xs' <- liftIO $ readIORef (dirs tree)
          case Map.lookup dir xs' of
            Nothing -> error "invalid directory"
            Just t  -> do
              res <- go t
              liftIO $ modifyIORef (dirs tree) (Map.insert dir res)
              go tree
        addDir dir = do
          r1 <- liftIO $ newIORef Map.empty
          r2 <- liftIO $ newIORef []
          liftIO $ modifyIORef (dirs tree) (Map.insert dir (FileTree r1 r2))
          go tree
        addFile bytes file = do
          liftIO $ modifyIORef (files tree) ((file, bytes):)
          go tree

getFileSizes :: FileTreePure -> (Int, (Int, [Int]))
getFileSizes tree = runState (go tree) (0, [])
  where
    go (FileTree dirs files) = do
      res <- (+ (sum $ map snd files)) . sum <$> mapM go dirs
      when (res <= 100000) $ modify (first (+res))
      modify (second (res:))
      return res

day7 :: IO ()
day7 = do
    xs <- lines <$> readFile "input/day7"
    tree <- toFileTree xs >>= purifyTree
    -- printFileTree 0 tree
    putStr "Part1: "
    let (fileSize, (part1, part2)) = getFileSizes tree
    print part1
    putStr "Part2: "
    print (minimum $ filter (>= (fileSize - 40000000)) part2)

-- just for lulz
printFileTree :: Int -> FileTreePure -> IO ()
printFileTree i (FileTree dirs files) = do
  sequence_ (Map.mapWithKey printDir dirs)
  mapM_ printFile files
  where
    printDir s t = do
      putStrLn $ replicate i ' ' ++ "- " ++ s ++ "(dir)"
      printFileTree (i+1) t
    printFile (f, s) =
      putStrLn $ replicate i ' ' ++ "- " ++ f ++ "(" ++ show s ++ ")"
