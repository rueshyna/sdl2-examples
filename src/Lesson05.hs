{- blit optmized image with scaling -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson05 where
--
import qualified SDL
import Linear.V2 (V2(V2))
import Linear.Affine(Point(P))
--
import PixelPen
--

lesson05 :: IO ()
lesson05
    = (^.^) sdlInitVideo ()                 -- initialize SDL
    $ \() -> (^.^) defaultWindow "Lesson05"   -- create window
    $ \w -> (^.^) surface w            -- get surface from given window
    $ \s -> (^.^) optBmpSurface (s, "./img/helloWorld.bmp")
    $ \o -> update $ eventHandle checkDefaultQuit
    $ \e -> do
      -- blit with given scaling setup
      -- Nothing for default setup - blitting with fully filling
      SDL.surfaceBlitScaled o Nothing s
        $ Just $ SDL.Rectangle (P (V2 0 0)) (V2 400 300)
      SDL.updateWindowSurface w
