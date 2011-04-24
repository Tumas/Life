module Life
(
  Universe,
  Position,
  randomUniverse, 
  nextUniverse,
  dimensionY,
  dimensionX, 
  activePositions,

  getAPG,
  getB,
  getNCount
)
where 

-- hacky and quite ugly attempt to save some memory
--  TODO: test 
--        optimize

import Data.Bits
import qualified Random (StdGen, random, randomRs)

type Grid = Int
type Position= (Int, Int)
type Universe = [[Grid]]

gridDimensionX :: Universe -> Int
gridDimensionX = length . head

gridDimensionY :: Universe -> Int
gridDimensionY = length 

dimensionX :: Universe -> Int 
dimensionX = ((*) 3) . gridDimensionX 

dimensionY :: Universe -> Int 
dimensionY = ((*) 3) . gridDimensionY 

getB :: Grid -> Int -> Int
getB g p = (shiftR g p) .&. (1::Int)

--getGrid :: Int -> Int -> Universe -> Position-> Grid
--getGrid mx my u (-1, -1) = (!!) ((!!) u my) mx
--getGrid mx my u (-1, gy) = (!!) ((!!) u gy) mx
--getGrid mx my u (gx, -1) = (!!) ((!!) u my) gx
--getGrid mx my u (gx, gy) = (!!) ((!!) u (gy `mod` (my+1))) (gx `mod` (mx+1)) 

getGrid mx my u gp@(gx, gy) = (!!) ((!!) u (gy `mod` (my+1))) (gx `mod` (mx+1))

applyRules :: Int -> Int -> Int 
applyRules 0 3 = 1
applyRules 0 _ = 0
applyRules 1 2 = 1
applyRules 1 3 = 1
applyRules 1 _ = 0

maxX :: Universe -> Int
maxX u = (gridDimensionX u)-1

maxY :: Universe -> Int
maxY u = (gridDimensionY u)-1

countCornerEdge :: Int -> Grid -> Grid -> Grid -> Grid -> Int
countCornerEdge l g h v c 
  | l == 0 = sum [(gg 7), (gg 5), (gg 4), (gh 3), (gh 6), (gc 0), (gv 2), (gv 1)]
  | l == 1 = sum [(gg 7), (gg 4), (gg 3), (gh 8), (gh 5), (gc 2), (gv 1), (gv 0)]
  | l == 2 = sum [(gg 5), (gg 1), (gg 4), (gh 3), (gh 0), (gc 6), (gv 8), (gv 7)]
  | l == 3 = sum [(gg 1), (gg 3), (gg 4), (gh 2), (gh 5), (gc 8), (gv 6), (gv 7)]
  where gg = getB g
        gh = getB h
        gc = getB c
        gv = getB v

countMiddleEdge :: Int -> Grid -> Grid -> Int
countMiddleEdge l g n
  | l == 0 = sum [(gg 3), (gg 5), (gg 4), (gg 8), (gg 6), (gn 0), (gn 2), (gn 1)]
  | l == 1 = sum [(gg 0), (gg 1), (gg 4), (gg 7), (gg 6), (gn 8), (gn 2), (gn 5)]
  | l == 2 = sum [(gg 0), (gg 2), (gg 3), (gg 4), (gg 5), (gn 6), (gn 7), (gn 8)]
  | l == 3 = sum [(gg 1), (gg 2), (gg 4), (gg 8), (gg 7), (gn 0), (gn 3), (gn 6)]
  where gg = getB g
        gn = getB n

getNCount :: Int -> Int -> Universe -> Grid -> Position-> Position-> (Int, Int)
getNCount mx my u g (gx, gy) lp@(lx, ly)
  | lp == (0, 0) = ((gg 8), countCornerEdge 0 g (g' (gx-1, gy)) (g' (gx, gy-1)) (g' (gx-1, gy-1)))
  | lp == (0, 1) = ((gg 5), countMiddleEdge 3 g (g' (gx-1, gy)))
  | lp == (0, 2) = ((gg 2), countCornerEdge 2 g (g' (gx-1, gy)) (g' (gx, gy+1)) (g' (gx-1, gy+1)))
  | lp == (1, 0) = ((gg 7), countMiddleEdge 0 g (g' (gx, gy-1)))
  | lp == (1, 1) = ((gg 4), (sum $ map (getB g) [0, 1, 2, 3, 5, 6, 7, 8]))
  | lp == (1, 2) = ((gg 1), countMiddleEdge 2 g (g' (gx, gy+1)))
  | lp == (2, 0) = ((gg 6), countCornerEdge 1 g (g' (gx+1, gy)) (g' (gx, gy-1)) (g' (gx+1, gy-1)))
  | lp == (2, 1) = ((gg 3), countMiddleEdge 1 g (g' (gx+1, gy)))
  | lp == (2, 2) = ((gg 0), countCornerEdge 3 g (g' (gx+1, gy)) (g' (gx, gy+1)) (g' (gx+1, gy+1)))
  where g' = getGrid mx my u
        gg = getB g

next :: Int -> Int -> Universe -> Position-> Grid -> Grid
next mx my u (gx, gy) g = snd $ foldr (\(v, nc) (i, n) -> (i+1, if (applyRules v nc) == 1 then setBit (n::Int) i else n)) (0, 0) values 
  where 
        values = map (getNCount' (gx, gy)) [(x,y) | y <- [0..2], x <- [0..2]]
        getNCount' = getNCount mx my u g 

nextUniverse :: Universe -> Universe
nextUniverse u = fst $ foldr processRow ([], 0) u
  where 
    processRow row (acc, y) = ((fst $ foldr (\g (acc', x) -> ( (next' (x, y) g) : acc', x+1)) ([], 0) row) : acc, y+1)
    next' = next (maxX u) (maxY u) u

randomUniverse :: Random.StdGen -> Int -> Int -> Universe
randomUniverse _ _ 0  = []
randomUniverse g x y  = 
  let (_, newGen) = Random.random g :: (Int, Random.StdGen)
  in (take x $ Random.randomRs (0, 511) g :: [Int]) : randomUniverse newGen x (y-1) 

activePositions :: Universe -> [Position]
activePositions u = fst $ foldr findActive ([], 0) u
  where findActive r (acc, y) = ((fst $ foldr (\g (acc', x) -> ((getAPG (x, y) g)++ acc', x+1)) ([], 0) r) ++ acc, y+1)

-- get active positions in grid
getAPG :: Position -> Grid -> [Position]
getAPG (gx, gy) 0 = []
getAPG (gx, gy) g = filter (\(x, y) -> x >= 0) $ zipWith zipF pos coords
  where
    coords = [(x', y') | y' <- [2,1..0], x' <- [2,1..0]]
    pos    = map (getB g) [0..8]
    zipF 0 (lx, ly) = (-1, -1) 
    zipF 1 (lx, ly) = (gx*3+lx, gy*3+ly)
