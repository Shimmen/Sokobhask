module Main where

import Prelude hiding (Either(..))


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

type Vec2 = (Int, Int)

data SokobanBoard = SokobanBoard { sbSize    :: Vec2   -- size of board
                                 , sbWalls   :: [Vec2] -- wall positions
                                 , sbBoxes   :: [Vec2] -- box positions
                                 , sbTargets :: [Vec2] -- target position
                                 , sbPlayer  :: Vec2   -- player position
                                 } deriving (Eq)

instance Show SokobanBoard where
  show = showBoard

readBoard :: String -> SokobanBoard
readBoard = undefined

showBoard :: SokobanBoard -> String
showBoard = undefined



main :: IO ()
main = do
  input <- getInput
  case input of
    Up -> putStrLn "Yay, you won!"
    _  -> main














