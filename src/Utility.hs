{-# LANGUAGE OverloadedStrings #-}
module Utility where

import qualified SDL
import qualified Config
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)

sdlInit :: (MonadIO m) => (() -> m (), () -> m ())
sdlInit = (const $ SDL.initialize [SDL.InitVideo], const SDL.quit)

window :: (MonadIO m) => (T.Text -> m SDL.Window, SDL.Window -> m ())
window = (flip SDL.createWindow Config.winConfig, SDL.destroyWindow)

surface :: (MonadIO m) => (SDL.Window -> m SDL.Surface, SDL.Surface -> m())
surface = (SDL.getWindowSurface, SDL.freeSurface)

loadBmpPic :: (MonadIO m) => (FilePath -> m SDL.Surface, SDL.Surface -> m())
loadBmpPic = (SDL.loadBMP, SDL.freeSurface)

(^.^) :: (MonadIO m) => (a -> m b, b -> m ()) -> a -> (b -> m c) -> m ()
(^.^) (x,y) i f = do
   c <- x i
   f c
   y c

