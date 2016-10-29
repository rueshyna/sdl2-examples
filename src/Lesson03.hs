{- Basic events handling -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson03 where
--
import qualified SDL
import Linear.V4 (V4(..))
--
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
--
import qualified Config
--

lesson03 :: IO ()
lesson03 = do
   -- initialize SDL
   SDL.initialize [SDL.InitVideo]

   -- create window
   window <- SDL.createWindow "Lesson03" Config.winConfig

   -- get surface from given window
   gSurface <- SDL.getWindowSurface window

   -- load image file as a surface
   pictureS <- SDL.loadBMP "./img/03/Broom.bmp"

   -- define the main loop
   let
      loop = do
         -- fetch all events from events pool
         events <- SDL.pollEvents
         -- check the existence of QuitEvent
         let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

         -- blit(copy/show) image surface onto window surface
         SDL.surfaceBlit pictureS Nothing gSurface Nothing

         -- update the surface
         SDL.updateWindowSurface window
         unless quit loop

   -- exec our main loop
   loop
   SDL.destroyWindow window
   SDL.freeSurface pictureS
   SDL.quit
