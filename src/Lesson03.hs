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
import Utility
--

lesson03 :: IO ()
lesson03
    = (^.^) sdlInit ()                 -- initialize SDL
    $ \() -> (^.^) window "Lesson03"   -- create window
    $ \w -> (^.^) surface w            -- get surface from given window
    $ \s -> (^.^) loadBmpPic "./img/hellowworld.bmp" -- load image file as a surface
    $ \p -> do

      let
         loop = do
            -- fetch all events from events pool
            events <- SDL.pollEvents
            -- check the existence of QuitEvent
            let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

            --blit(copy/show) image surface onto window surface-}
            SDL.surfaceBlit p Nothing s Nothing

            -- update the surface
            SDL.updateWindowSurface w
            unless quit loop

      -- exec our main loop
      loop
