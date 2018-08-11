module Trans where


import qualified Data.Map as Map
import Data.Maybe
import Data.List (notElem, lookup)
import Prelude


data BoardState =
  BoardState Int
             [Play]


data GameState
  = New
  | Waiting Player
  | Playing Int
            [Play]
            Player
            Player
            Color
  | Finished Color
  deriving (Show)

type Play = ((Int, Int), Color)

data Color
  = Red
  | Blue
  deriving (Show, Eq)

data Player
  = Human Color
  | Computer Color
  deriving (Show)


addPlayer :: GameState -> Player -> GameState
addPlayer New player = Waiting player
addPlayer (Waiting player) newPlayer = Playing 11 [] player newPlayer Red
addPlayer (Playing s plays p op c) _ = Playing s plays p op c
addPlayer (Finished p) _ = Finished p


drawColor :: Color -> String
drawColor Red = "R"
drawColor Blue = "B"


drawBoard :: Int -> [Play] -> String
drawBoard size plays =
  concat
    [ '\n' :
    [' ' | _ <- [1 .. y]] ++
    unwords [maybe "_" drawColor (lookup (x, y) plays) | x <- [0 .. (size - 1)]]
    | y <- [0 .. (size - 1)]
    ]


newGame :: GameState
newGame = New


neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) =
  map
    (\(xoff, yoff) -> (x + xoff, y + yoff))
    [(0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1)]


pathAt :: [Play] -> Color -> (Int, Int) -> [(Int, Int)]
pathAt plays color point =
  let check leaves_ path =
        case leaves_ of
          [] -> path
          leaf:leaves ->
            let newLeaves =
                  filter
                    (\p -> p `notElem` path && lookup p plays == Just color) $
                  neighbors leaf
             in check (newLeaves ++ leaves) (newLeaves ++ path)
   in check [point] []


winningPath :: [Play] -> Color ->  Int -> Bool
winningPath [] _ _ = False
winningPath plays color boardSize = elem 0 cells && elem boardSize cells
  where
    cells = map (lookupFor color) plays

lookupFor :: Color -> (Play -> Int)
lookupFor color = l
  where
    l (c, _) =
      if color == Blue
        then fst c
        else snd c


allOccupied :: BoardState -> Color -> Int
allOccupied (BoardState size plays) color =
  sum $ map (\n -> if n `elem` cells then 10 else 0) [0..size]
  where cells = map (lookupFor color) plays


main :: IO ()
main =
  putStrLn $
  drawBoard
    11
    [ ((2, 1), Red)
    , ((3, 5), Blue)
    , ((1, 2), Red)
    , ((3, 6), Blue)
    , ((9, 2), Red)
    , ((7, 6), Blue)
    , ((8, 3), Red)
    , ((6, 6), Blue)
    , ((4, 3), Red)
    , ((2, 5), Blue)
    , ((10, 2), Red)
    ]
