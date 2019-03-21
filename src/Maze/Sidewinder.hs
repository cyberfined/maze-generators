-- Maze.Sidewinder v 0.1
-- (c) cyberfined 2018 | https://github.com/cyberfined/maze-generators

module Maze.Sidewinder (
  genMaze -- Generate maze
  ) where

import System.Random
import Prelude hiding(Left, Right)
import Maze.Types

-- Generate maze, which size w x h, with two directions
genMaze :: StdGen -> (Direction, Direction) -> Int -> Int -> Maze
genMaze gen dirs w h = fst $ foldr (\c (maze,g) -> genColumn g dirs w h c [] maze) (initMaze,gen) columns
  where initMaze = emptyMaze w h
        columns = getColumnsCoords w h

-- Generate one column. Return maze and new stdgen
genColumn :: StdGen -> (Direction, Direction) -> Int -> Int -> [(Int, Int)] -> [(Int, Int)] -> Maze -> (Maze, StdGen)
genColumn gen dirs w h (c:cols) set maze
  | ind == 0  = fstMaybe $ maybe (maze,gen') sndNext sndWay
  | otherwise = sndMaybe $ maybe (maze,gen') fstNext fstWay 
  where fstWay = randomPass gen' w h newSet (fst dirs) maze
        fstNext (maze',gen'') = genColumn gen'' dirs w h cols [] maze'
        fstMaybe f = maybe f fstNext fstWay
        
        sndWay = openPass w h c (snd dirs) maze
        sndNext maze' = genColumn gen' dirs w h cols newSet maze'
        sndMaybe f = maybe f sndNext sndWay
        
        newSet = c:set
        (ind, gen') = randomR (0,1) gen :: (Int, StdGen)
genColumn gen _ _ _ _ _ maze = (maze,gen)

-- Choose one cell. If possible open pass by given direction and return Just (new_maze, new_stdgen), else return Nothing
randomPass :: StdGen -> Int -> Int -> [(Int, Int)] -> Direction -> Maze -> Maybe (Maze, StdGen)
randomPass gen w h sets dir maze
  | allowedL > 0 = let (ind, gen') = randomR (0,allowedL-1) gen in (allowed !! ind) >>= \m -> Just (m,gen')
  | otherwise = Nothing
  where allowed = filter (/=Nothing) $ map (\c -> openPass w h c dir maze) sets
        allowedL = length allowed

-- Return all cell's coordinates grouped by column
getColumnsCoords :: Int -> Int -> [[(Int, Int)]]
getColumnsCoords w h = [[(x,y) | x <- [0..w-1]] | y <- [0..h-1]]
