{-  Load and display image via surface -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson02 where
--
import qualified SDL
import Linear.V4 (V4(..))
--
import Control.Concurrent (threadDelay)
--
import PixelPen
--

lesson02 :: IO ()
lesson02
    = (^.^) sdlInitVideo ()                          -- initialize SDL
    $ \() -> (^.^) defaultWindow "Lesson02"          -- create window
    $ \w -> (^.^) surface w                          -- get surface from given window
    $ \s -> (^.^) bmpSurface "./img/helloWorld.bmp"  -- load image file as a surface
    $ \p -> do

      -- blit(copy/show) image surface onto window surface
      SDL.surfaceBlit p Nothing s Nothing

      -- update the surface
      SDL.updateWindowSurface w

      -- wait two seconds
      threadDelay 2000000
