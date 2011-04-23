module LifeGraphics where

import Random
import Data.Bits
import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)
import GHC.Int

import qualified Life 

-- make it really random

main :: IO ()
main = do
  dpy <- openDisplay ""
  let dflt   = defaultScreen dpy
      scr      = defaultScreenOfDisplay dpy
      universe = Life.randomUniverse (mkStdGen 13) 80 80 
  rootw <- rootWindow dpy dflt
  win   <- mkUnmanagedWindow dpy scr rootw 0 0 800 800
  fgcolor <- initColor dpy "green"
  gc <- createGC dpy win
  setForeground dpy gc fgcolor

  mapWindow dpy win
  updateWin dpy win gc universe

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros, real) <- allocNamedColor dpy colormap color 
  return $ color_pixel apros

getFillAction :: Display -> Drawable -> GC -> Dimension -> Dimension -> Life.Position -> IO ()
getFillAction dpy win gc w h a = fillRectangle dpy win gc (fromIntegral x) (fromIntegral y) w h
  where x = (fromIntegral $ fst a) * w 
        y = (fromIntegral $ snd a) * h

updateWin :: Display -> Window -> GC -> Life.Universe -> IO ()
updateWin dpy win gc u = do
  let next_universe = Life.nextUniverse u
  sequence $ map (getFillAction dpy win gc 10 10) $ Life.activePositions u
  sync dpy False
  threadDelay (1 * 250000)
  clearWindow dpy win
  updateWin dpy win gc next_universe 

mkUnmanagedWindow :: Display
 -> Screen
 -> Window
 -> Position
 -> Position
 -> Dimension
 -> Dimension
 -> IO Window
mkUnmanagedWindow dpy scr rw x y w h = do
 let visual = defaultVisualOfScreen scr
     attrmask = cWOverrideRedirect .|. cWBorderPixel .|. cWBackPixel
 background_color <- initColor dpy "black"
 border_color <- initColor dpy "green"
 win <- allocaSetWindowAttributes $ \attributes -> do
     set_override_redirect attributes True
     set_background_pixel attributes background_color
     set_border_pixel attributes border_color
     createWindow dpy rw x y w h 1 (defaultDepthOfScreen scr) inputOutput visual attrmask attributes
 return win
