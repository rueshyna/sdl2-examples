{- Basic Window Setup and Display  -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson01 where
--
import qualified SDL
import Linear.V4 (V4(..))
--
import Control.Concurrent (threadDelay)
--
import Utility
--

lesson01 :: IO ()
lesson01
    = (^.^) sdlInit ()                 -- initialize SDL
    $ \() -> (^.^) window "Lesson01"   -- create window
    $ \w -> (^.^) surface w            -- get surface from given window
    $ \s -> do

       -- fill the global surface with black
       SDL.surfaceFillRect s Nothing $
         -- setting color with R-G-B-A
         V4 maxBound maxBound minBound maxBound

       -- update the surface for a specific window
       SDL.updateWindowSurface w

       -- wait two seconds :
       -- https://hackage.haskell.org/package/sdl2-2.1.2.1/docs/SDL-Time.html#v:delay
       -- Users are generally recommended to use threadDelay instead, to take
       -- advantage of the abilities of the Haskell runtime.-}
       threadDelay 2000000
