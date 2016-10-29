{-  Load and display image via surface -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson02 where
--
import qualified SDL
import Linear.V4 (V4(..))
--
import Control.Concurrent (threadDelay)
--
import qualified Config
--

lesson02 :: IO ()
lesson02 = do
   -- initialize SDL
   SDL.initialize [SDL.InitVideo]

   -- create window
   window <- SDL.createWindow "Lesson02" Config.winConfig

   -- get surface from given window
   gSurface <- SDL.getWindowSurface window

   -- load image file as a surface
   pictureS <- SDL.loadBMP "./img/02/Broom.bmp"

   -- blit(copy/show) image surface onto window surface
   SDL.surfaceBlit pictureS Nothing gSurface Nothing

   -- update the surface
   SDL.updateWindowSurface window

   -- wait two seconds
   threadDelay 2000000

   -- free surface
   SDL.freeSurface pictureS

   -- destroy window
   SDL.destroyWindow window

   -- quit SDL subsystems
   SDL.quit
