-- Maze.Wilson v 0.1
-- (c) cyberfined 2018 | https://github.com/cyberfined/maze-generators

module Maze.Wilson (
  genMaze -- Generate maze
  ) where

import System.Random
import Data.List(findIndex)
import Data.Maybe(fromJust)
import Prelude hiding (Left,Right)
import Maze.Types

-- Generate maze, which size w x h
genMaze :: StdGen -> Int -> Int -> Maze
genMaze gen w h = fst $ genMaze' gen' w h [c] initMaze
  where (c, gen') = genCellCoords gen w h
        initMaze = emptyMaze w h

genMaze' :: StdGen -> Int -> Int -> [(Int, Int)] -> Maze -> (Maze, StdGen)
genMaze' gen w h set maze
  | length set == w*h = (maze, gen)
  | c `elem` set = genMaze' gen' w h set maze
  | otherwise = genMaze' gen'' w h newSet maze'
  where (Just maze') = openPassBySubset w h subset maze
        newSet = set ++ xs
        (subset@(_:xs), gen'') = genSubset gen' w h set [c]
        (c,gen') = genCellCoords gen w h

-- Generate subset
genSubset :: StdGen -> Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> ([(Int, Int)], StdGen)
genSubset gen w h set subset@(c:_)
  | isOutOfBorder w h neigh = genSubset gen' w h set subset
  | neigh `elem` set = (nextSubset, gen')
  | isNInSubset /= Nothing = genSubset gen' w h set dropedSubset
  | otherwise = genSubset gen' w h set nextSubset
  where dirs = [Up,Down,Left,Right]
        neigh = getNeighCell (dirs !! ind) c
        isNInSubset = findIndex (==neigh) subset
        nextSubset = neigh:subset
        dropedSubset = drop (fromJust isNInSubset) subset
        (ind,gen') = randomR (0,3) gen

-- Open passes by subset consistently
openPassBySubset :: Int -> Int -> [(Int,Int)] -> Maze -> Maybe Maze
openPassBySubset w h (a:b:xs) maze =
  direction a b >>= (\d -> openPass w h a d maze >>= openPassBySubset w h (b:xs))
openPassBySubset _ _ _ maze = Just maze
