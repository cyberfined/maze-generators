-- Maze.BTree v 0.1
-- (c) cyberfined 2018 | https://github.com/cyberfined/maze-generators

module Maze.BTree (
  genMaze -- Generate maze
  ) where

import Control.Monad(mplus)
import Prelude hiding(Left, Right)
import System.Random
import Maze.Types

-- Generate maze, which size w x h, with two directions
genMaze :: StdGen -> (Direction, Direction) -> Int -> Int -> Maze
genMaze gen dirs w h = fst $ foldr (\c (maze,g) -> genCell g dirs w h c maze) (initMaze,gen) coords
  where initMaze = emptyMaze w h
        coords = getMazeCoords w h

-- Open one of two passes, which given in tuple, in cell. Return maze and new stdgen
genCell :: StdGen -> (Direction, Direction) -> Int -> Int -> (Int, Int) -> Maze -> (Maze, StdGen)
genCell gen dirs w h c maze =
  case (attempt1 `mplus` attempt2) of
    Nothing -> (maze, gen')
    Just maze' -> (maze', gen')
  where attempt1 = openPass w h c (getDir !! ind1 $ dirs) maze
        attempt2 = openPass w h c (getDir !! ind2 $ dirs) maze
        (ind1, gen') = randomR (0,1) gen
        ind2 = 1 - ind1
        getDir = [fst,snd]
