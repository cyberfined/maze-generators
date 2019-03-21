-- Main v 0.1
-- (c) cyberfined 2018 | https://github.com/cyberfined/maze-generators

module Main where

import System.Random
import Control.Monad(zipWithM_)
import qualified Maze.Types as T
import qualified Maze.BTree as BMaze
import qualified Maze.Sidewinder as SMaze
import qualified Maze.AldousBroder as AMaze
import qualified Maze.Wilson as WMaze

-- Get functions for maze generating and print all mazes
testFunc :: [(StdGen -> T.Maze)] -> IO ()
testFunc = zipWithM_ (\gf mf -> gf >>= (\g -> print $ mf g)) (repeat newStdGen)
  
main :: IO ()
main = do
  testFunc [ (\g -> BMaze.genMaze g (T.Up, T.Right) w h)
           , (\g -> SMaze.genMaze g (T.Up, T.Right) w h)
           , (\g -> AMaze.genMaze g w h)
           , (\g -> WMaze.genMaze g w h)
           ]
  where w = 30
        h = 12
