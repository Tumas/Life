module LifeGraphics where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

import Random
import Data.IORef
import qualified Life 

cellWidth = 10

data LifeInfo = LifeInfo { 
    universe :: Life.Universe,
    orthoSize :: Size
  }

main = do
  (progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  createWindow progName
  -- wrong
  sz <- get windowSize
  let width = outermostX' cellWidth sz
  matrixMode $= Projection
  loadIdentity
  setOrtho width

  cColor <- randomColor
  currentColor $= Color4 (head cColor) (head $ tail cColor) (last cColor) 1

  universeM <- createRandomUniverse' ((\(Size x y)-> Size (x*2)(y*2)) width) 
  universe <- newIORef LifeInfo { universe = universeM, orthoSize = width }
  displayCallback $= display universe 
  mainLoop

randomColor = do
  gen <- newStdGen
  return (map (\x -> (fromIntegral(x) :: Float) / 100) 
    $ take 3 $ randomRs (0 :: Int, 90) gen)
  
{- 300 x 300 
 -  300 / 10 $ / 2 = 15 
 -    coordinates: -15 .. 15
 -    universe: 30 x 30
 -}
outermostX' :: GLsizei -> Size -> Size 
outermostX' w (Size x y) = Size ( x `div` w `div` 2 ) (y `div` w `div` 2)

setOrtho (Size x y) = ortho (cc(-x)) (cc(x)) (cc(-y)) (cc(y)) (cc(-x)) (cc(x))
  where cc t = (fromIntegral t) :: GLdouble

createRandomUniverse (Size x y) = Life.randomUniverse (mkStdGen 0) (fromIntegral x) (fromIntegral y)
createRandomUniverse' (Size x y) = do
  gen <- getStdGen
  return (Life.randomUniverse gen (fromIntegral x) (fromIntegral y))

idle universeInfo = do
  uInfo <- get universeInfo
  let u = universe uInfo
  universeInfo $= uInfo { universe = Life.nextUniverse u, orthoSize = orthoSize uInfo }
  postRedisplay Nothing

display universeInfo = do
  clear [ColorBuffer]
  uInfo <- get universeInfo
  let u = universe uInfo
  sequence $ map (displayCell (orthoSize uInfo)) $ Life.activePositions u

  flush
  swapBuffers
  addTimerCallback 300 $ idle universeInfo

displayCell (Size xs ys) (x, y) = do 
  renderPrimitive Quads $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) points
    where points = [(xx, yy, 0), (xx+1, yy, 0), (xx+1, yy-1, 0), (xx, yy-1, 0)]
          xx = fromIntegral(x) - xs
          yy = ys - fromIntegral(y)

displayAt s (x, y) displayFunc = preservingMatrix $ do 
  translate $ Vector3 x y 0
  displayFunc s (x, y)

{- 
 - TODO:
 -  1. replace naive algorithm
 -  2. find and replace stale state 
 -  3. patterns 
 -}
