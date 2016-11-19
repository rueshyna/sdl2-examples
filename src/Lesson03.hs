{- Basic events handling -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson03 where
--
import qualified SDL
import Linear.V4 (V4(..))
--
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (unless)
--
import PixelPen
--

lesson03 :: IO ()
lesson03
    = (^.^) sdlInitVideo ()                         -- initialize SDL
    $ \() -> (^.^) defaultWindow "Lesson03"         -- create window
    $ \w -> (^.^) surface w                         -- get surface from given window
    $ \s -> (^.^) bmpSurface "./img/helloWorld.bmp" -- load image file as a surface
    $ \p -> update $ eventHandle checkDefaultQuit
    $ \e -> do
        {- blit(copy/show) image surface onto window surface -}
        SDL.surfaceBlit p Nothing s Nothing

        {- update the surface -}
        SDL.updateWindowSurface w
