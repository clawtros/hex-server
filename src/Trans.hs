module Trans where


import qualified Data.Map as Map
import Data.Maybe
import qualified Data.List as List

data GameState =
  New
  | Waiting Player
  | Playing Int [Play] Player Player Color
  | Finished Color
  deriving (Show)

type Play = ((Int, Int), Color) 

data Color = Red | Blue
  deriving (Show)

data Player = Human Color | Computer Color
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
  concat ['\n' : [' ' | _ <- [1..y]] ++
          List.intercalate " "
           [maybe "_" drawColor (Map.lookup (x, y) playMap)
           | x <- [0..(size - 1)]]
         | y <- [0..(size - 1)]]
  where playMap = Map.fromList plays


newGame :: GameState
newGame = 
  New


neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x, y) =
  map (\(xoff, yoff) -> (x + xoff, y + yoff))
  [(0, (-1)), (1, (-1)), ((-1), 0), (1, 0), ((-1), 1), (0, 1)]
  

main :: IO ()
main =
  putStrLn $ drawBoard 11 [ ((2, 1), Red)
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