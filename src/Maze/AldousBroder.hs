-- Maze.AldousBroder v 0.1
-- (c) cyberfined 2018 | https://github.com/cyberfined/maze-generators

module Maze.AldousBroder (
  genMaze -- Generate maze
  ) where

import System.Random
import Prelude hiding(Left,Right)
import Maze.Types

-- Generate maze, which size w x h
genMaze :: StdGen -> Int -> Int -> Maze
genMaze gen w h = fst $ genMaze' gen' w h c [c] initMaze
  where initMaze = emptyMaze w h
        (c,gen') = genCellCoords gen w h
        
genMaze' :: StdGen -> Int -> Int -> (Int, Int) -> [(Int, Int)] -> Maze -> (Maze, StdGen)
genMaze' gen w h c set maze
  | length set == w*h = (maze,gen)
  | isOutOfBorder w h neigh = genMaze' gen' w h c set maze
  | neigh `notElem` set = genMaze' gen' w h neigh (neigh:set) maze'
  | otherwise = genMaze' gen' w h neigh set maze
  where dirs = [Up,Down,Left,Right]
        dir = dirs !! ind
        neigh = getNeighCell dir c
        (Just maze') = openPass w h c dir maze
        (ind,gen') = randomR (0,3) gen
