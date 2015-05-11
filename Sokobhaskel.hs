module Main where

import Prelude hiding (Either(..))

{-

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

-}

type Coord = (Int, Int)

data Level = Level { lvlSize :: Coord
                   , lvlWalls :: [Coord]
                   }
                   deriving (Eq)

instance Show Level where
  show = showLevel

setTile :: Level -> (Coord, Char) -> Level
setTile lvl (coord, '#' ) = lvl{lvlWalls = coord:lvlWalls lvl}
setTile lvl (_    , ' ' ) = lvl
setTile _   (_    , char) = error (show char ++ " not recognized")

getTile :: Level -> Coord -> Char
getTile lvl coord | isWall lvl coord = '#'
                  | otherwise        = ' '

isWall :: Level -> Coord -> Bool
isWall lvl coord = coord `elem` lvlWalls lvl

emptyLevel :: Level
emptyLevel = Level { lvlSize  = (0, 0)
                   , lvlWalls = []
                   }

readLevel :: String -> Level
readLevel str = foldl setTile (emptyLevel{lvlSize = maxi}) elems
  where
    lns     = lines str
    coords  = [[(x,y) | x <- [0..]] | y <- [0..]]
    elems   = concat $ zipWith zip coords lns
    maxX    = maximum . map (fst . fst) $ elems
    maxY    = maximum . map (snd . fst) $ elems
    maxi    = (maxX, maxY)

showLevel :: Level -> String
showLevel lvl = unlines chars
  where
    (maxX, maxY) = lvlSize lvl
    chars        = [[getTile lvl (x, y) | x <- [0..maxX]]
                                        | y <- [0..maxY]]

main :: IO ()
main = do
  levelContents <- readFile "level1.lvl"
  let level = readLevel levelContents
  print level


















