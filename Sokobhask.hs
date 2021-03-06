module Main (main) where

import Prelude hiding (Left, Right)
import System.IO
import Data.List

----------
-- data --
----------

type Coord = (Int, Int)

data Input = Left
           | Right
           | Up
           | Down

data Level = Level {
  lvlSize    :: Coord
, lvlPlayer  :: Coord
, lvlWalls   :: [Coord]
, lvlCrates  :: [Coord]
, lvlStorage :: [Coord]
} deriving (Eq)

instance Show Level where
  show = showLevel

emptyLevel :: Level
emptyLevel = Level {
  lvlSize    = (0, 0)
, lvlPlayer  = (0, 0)
, lvlWalls   = []
, lvlCrates  = []
, lvlStorage = []
}

-----------
-- logic --
-----------

move :: Coord -> Input -> Coord
move (x, y) Left  = (x - 1, y)
move (x, y) Right = (x + 1, y)
move (x, y) Up    = (x, y - 1)
move (x, y) Down  = (x, y + 1)

addTile :: Level -> (Coord, Char) -> Level
addTile lvl (c, '#' ) = lvl{lvlWalls  = c:lvlWalls lvl}
addTile lvl (c, '*' ) = lvl{lvlCrates = c:lvlCrates lvl}
addTile lvl (c, 'o' ) = lvl{lvlStorage = c:lvlStorage lvl}
addTile lvl (c, '@' ) = lvl{lvlPlayer = c}
addTile lvl (_, ' ' ) = lvl
addTile _   (_, char) = error (show char ++ " not recognized")

getTile :: Level -> Coord -> Char
getTile lvl c
  | isPlayer        lvl c = '@'
  | isWall          lvl c = '#'
  | isFilledStorage lvl c = 'x'
  | isCrate         lvl c = '*'
  | isStorage       lvl c = 'o'
  | otherwise             = ' '

isWall :: Level -> Coord -> Bool
isWall lvl c = c `elem` lvlWalls lvl

isCrate :: Level -> Coord -> Bool
isCrate lvl c = c `elem` lvlCrates lvl

isStorage :: Level -> Coord -> Bool
isStorage lvl c = c `elem` lvlStorage lvl

isFilledStorage :: Level -> Coord -> Bool
isFilledStorage lvl c =
  isCrate lvl c && isStorage lvl c

isPlayer :: Level -> Coord -> Bool
isPlayer lvl coord =
  lvlPlayer lvl == coord

readLevel :: String -> Level
readLevel str = foldl addTile (emptyLevel{lvlSize = size}) tiles
  where
    coords  = [[(x, y) | x <- [0..]]
                       | y <- [0..]]
    tiles = concat $ zipWith zip coords (lines str)

    sizeX = maximum . map (fst . fst) $ tiles
    sizeY = maximum . map (snd . fst) $ tiles
    size  = (sizeX, sizeY)

showLevel :: Level -> String
showLevel lvl = unlines tileLines
  where
    (sizeX, sizeY) = lvlSize lvl
    tileLines = [[getTile lvl (x, y) | x <- [0..sizeX]]
                                     | y <- [0..sizeY]]

update :: Level -> Input -> Level
-- if the new pos is on a crate, move the crate too
update lvl inp =
  if isCrate lvl p'
    then lvl{lvlPlayer = p', lvlCrates = crates'}
    else lvl{lvlPlayer = p'}

  where
    p0 = lvlPlayer lvl
    p1 = p0 `move` inp
    p2 = p1 `move` inp

    -- the new player position
    p' | isWall lvl p1 || (isCrate lvl p1 && isCrate lvl p2) = p0
       | isCrate lvl p1 = if isWall lvl p2 then p0 else p1
       | otherwise = p1

    -- the new list of crates
    crates' = p2 : filter (/= p1) (lvlCrates lvl)

isCompleted :: Level -> Bool
isCompleted lvl = sort (lvlCrates lvl) == sort (lvlStorage lvl)

--------
-- IO --
--------

gameLoop :: Level -> IO ()
gameLoop lvl = do
  inp <- getInput
  let lvl' = update lvl inp
  renderToConsole lvl'
  if isCompleted lvl'
    then putStrLn "You won!"
    else gameLoop lvl'

loadLevel :: FilePath -> IO Level
loadLevel path = do
  s <- readFile $ "levels/" ++ path
  return $ readLevel s

renderToConsole :: Level -> IO()
renderToConsole =
  putStrLn . showLevel

getInput :: IO Input
getInput = do
  c <- getChar
  case c of
    'a' -> return Left
    'd' -> return Right
    'w' -> return Up
    's' -> return Down
    otherwise -> getInput

main :: IO ()
main = do

  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  lvl <- loadLevel "level1.lvl"
  renderToConsole lvl
  gameLoop lvl
