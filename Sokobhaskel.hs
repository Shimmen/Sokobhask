module Main where

import Prelude hiding (Either(..))


type Vec2 = (Int, Int)

data Input = Up
           | Down
           | Left
           | Right
           deriving (Show, Eq)

getInput :: IO Input
getInput = do
  char <- getChar
  case char of
    'w'       -> return Up
    's'       -> return Down
    'a'       -> return Left
    'd'       -> return Right
    otherwise -> getInput

add :: Vec2 -> Input -> Vec2
add (x,y) Up    = (x, y - 1)
add (x,y) Down  = (x, y + 1)
add (x,y) Left  = (x - 1, y)
add (x,y) Right = (x + 1, y)

data SokobanBoard = SokobanBoard { sbSize    :: Vec2   -- size of board
                                 , sbPlayer  :: Vec2   -- player position
                                 , sbWalls   :: [Vec2] -- wall positions
                                 , sbBoxes   :: [Vec2] -- box positions
                                 , sbTargets :: [Vec2] -- target position
                                 } deriving (Eq)

emptySokobanBoard :: SokobanBoard
emptySokobanBoard = SokobanBoard { sbSize    = (0, 0)
                                 , sbPlayer  = (0, 0)
                                 , sbWalls   = []
                                 , sbBoxes   = []
                                 , sbTargets = []
                                 }

isTile :: (SokobanBoard -> [Vec2]) -> SokobanBoard -> Vec2 -> Bool
isTile tileListProvider sb coord = coord `elem` tileListProvider sb

isWall   = isTile sbWalls
isBox    = isTile sbBoxes
isTarget = isTile sbTargets

instance Show SokobanBoard where
  show = showBoard

showBoard :: SokobanBoard -> String
showBoard = undefined

readBoard :: String -> SokobanBoard
readBoard = undefined



main :: IO ()
main = do
  input <- getInput
  print input


