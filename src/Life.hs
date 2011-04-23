module Life
(
  Universe,
  Position,
  randomUniverse, 
  nextUniverse,
  activePositions,
  dimensionY,
  dimensionX, 
  cellAt,
)
where 

import Random

type Cell = Int
type Universe = [[Cell]]
type Position = (Int, Int)

randomUniverse :: StdGen -> Int -> Int -> Universe
randomUniverse _ _ 0  = []
randomUniverse g x y  = 
  let (_, newGen) = random g :: (Int, StdGen)
  in (take x $ randomRs (0, 1) g :: [Cell]) : randomUniverse newGen x (y-1) 

dimensionX :: Universe -> Int
dimensionX = length . head

dimensionY :: Universe -> Int
dimensionY = length

cellAt :: Universe -> Position -> Cell
cellAt u (x, y) = (!!) ((!!) u y) x 
isVisible :: Universe -> Position -> Bool
isVisible u p = x >= 0 && x < maxX && y >= 0 && y < maxY  
  where maxX = dimensionX u
        maxY = dimensionY u
        x = fst p
        y = snd p
 
countNeighbours :: Universe -> Position -> Int
countNeighbours u p 
--  | not $ isVisible u p = 0
--  | otherwise           
  = sum . map (cellAt u) $ filter (isVisible u) cordinates
  where cordinates = [(x-1, y-1), (x, y-1), (x+1, y-1), (x-1, y), (x+1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]
        x = fst p 
        y = snd p


nextUniverse :: Universe -> Universe
nextUniverse u = foldr (processRow u) [] u 
  where processRow u r acc    = (foldr ((processCell u) (yy - (length acc))) [] r) : acc 
        processCell u y c acc = ((applyRules u c) (xx - (length acc), y)) : acc 
        xx = (dimensionX u) - 1
        yy = (dimensionY u) - 1

activePositions :: Universe -> [Position]
activePositions u = map (\e -> fst e) $
          filter (\e -> (snd e) == 1) $ zip coords $ concat u
  where coords = [(x, y) | x <- [0..xx], y <- [0..yy]] 
        xx = (dimensionX u) - 1
        yy = (dimensionY u) - 1

applyRules :: Universe -> Cell -> Position -> Cell
applyRules u c p  
  | count < 2   = 0
  | count > 3   = 0
  | count == 3  = 1
  | otherwise   = c
  where count = countNeighbours u p
