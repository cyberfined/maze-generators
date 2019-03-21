-- Maze.Types v 0.1
-- (c) cyberfined 2018 | https://github.com/cyberfined/maze-generators

module Maze.Types (
  Maze(..),      -- Maze data type
  Passes(..),    -- Data type for cell's allowed movements
  Direction(..), -- Directions for openning passes
  emptyMaze,     -- Create empty maze
  openPass,      -- Open one pass if possible
  getMazeCoords, -- Return all cell's coordinates
  modifyMatrix,  -- Update one element in matrix
  isOutOfBorder, -- Check if coordinate out of maze
  getNeighCell,  -- Return neighbour cell by direction
  direction,     -- Return direction between two cells
  genCellCoords  -- Generate cell's coordinates1
  ) where

import System.Random
import Data.List(transpose)
import Prelude hiding (Left, Right)

-- Cell's allowed movements
data Passes = Passes { up :: Bool, down :: Bool, left :: Bool, right :: Bool } deriving Eq

-- Maze
newtype Maze = Maze [[Passes]] deriving Eq

-- Show instance for Passes
instance Show Passes where
  show p = concatMap (++ "\n") $ cellView p

-- Show instance for Maze
instance Show Maze where
  show (Maze maze) = concatMap (concatMap (++ "\n")) lines
    where lines = map ((map concat) . transpose) viewMatrix
          viewMatrix = map (map cellView) maze

-- Directions for openning passes
data Direction = Up | Down | Left | Right

-- List of three strings to represent one cell
cellView :: Passes -> [String]
cellView (Passes u d l r) =
  [ " " ++ showBorder u ++ " "
  , showBorder l ++ "\x2588" ++ showBorder r
  , " " ++ showBorder d ++ " "
  ]
  where showBorder True = "\x2588" -- black space character
        showBorder _ = " "

-- Create maze, which size w x h. All movements forbidden
emptyMaze :: Int -> Int -> Maze
emptyMaze w h = Maze $ replicate h $ replicate w (Passes False False False False)

-- Open one pass if possible and return Just new_maze, else return Nothing
openPass :: Int -> Int -> (Int, Int) -> Direction -> Maze -> Maybe Maze
openPass w h (x,y) d (Maze maze) = case d of
  Up    -> if (isOutOfBorder w h (x,y-1)) then Nothing else Just $ Maze $ modifyMatrix opDown x (y-1) $ modifyMatrix opUp x y maze
  Down  -> if (isOutOfBorder w h (x,y+1)) then Nothing else Just $ Maze $ modifyMatrix opUp x (y+1) $ modifyMatrix opDown x y maze
  Left  -> if (isOutOfBorder w h (x-1,y)) then Nothing else Just $ Maze $ modifyMatrix opRight (x-1) y $ modifyMatrix opLeft x y maze
  Right -> if (isOutOfBorder w h (x+1,y)) then Nothing else Just $ Maze $ modifyMatrix opLeft (x+1) y $ modifyMatrix opRight x y maze
  where opUp p = p{up=True}
        opDown p = p{down=True}
        opLeft p = p{left=True}
        opRight p = p{right=True}

-- Return all cell's coordinates
getMazeCoords :: Int -> Int -> [(Int, Int)]
getMazeCoords w h
  | w <= 0 || h <= 0 = []
  | otherwise = [(x,y) | x <- [0..w-1], y <- [0..h-1]]

-- Update one element in matrix
modifyMatrix :: (a -> a) -> Int -> Int -> [[a]] -> [[a]]
modifyMatrix modify x y mat = modifyList (modifyList modify x) y mat

-- Update one element in list
modifyList :: (a -> a) -> Int -> [a] -> [a]
modifyList modify n lst =
  case (splitAt n lst) of
    (ini, cur:rest) -> ini ++ (modify cur:rest)
    _ -> lst

-- Check if coordinate out of maze
isOutOfBorder :: Int -> Int -> (Int, Int) -> Bool
isOutOfBorder w h (x,y) = x < 0 || x >= w || y < 0 || y >= h

-- Return neighbour cell by direction
getNeighCell :: Direction -> (Int, Int) -> (Int, Int)
getNeighCell d (x,y) = case d of
  Up    -> (x,y-1)
  Down  -> (x,y+1)
  Left  -> (x-1,y)
  Right -> (x+1,y)

-- Return direction between two cells
direction :: (Int, Int) -> (Int, Int) -> Maybe Direction
direction (x1,y1) (x2,y2)
  | x1 == x2 = Just $ if (y2 - y1 < 0) then Up else Down
  | y1 == y2 = Just $ if (x2 - x1 < 0) then Left else Right
  | otherwise = Nothing

-- Generate cell's coordinates
genCellCoords :: StdGen -> Int -> Int -> ((Int, Int), StdGen)
genCellCoords gen w h = ((rx,ry), gen'')
  where (rx, gen')  = randomR (0,w-1) gen
        (ry, gen'') = randomR (0,h-1) gen'
