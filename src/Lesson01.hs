{- Basic Window Setup and Display  -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson01 where
--
import qualified SDL
import Linear.V4 (V4(..))
--
import Control.Concurrent (threadDelay)
--
import qualified Config
--

lesson01 :: IO ()
lesson01 = do
   -- initialize SDL
   SDL.initialize [SDL.InitVideo]

   -- create window
   window <- SDL.createWindow "Lesson01" Config.winConfig

   -- get surface from given window
   gSurface <- SDL.getWindowSurface window

   -- fill the global surface with black
   SDL.surfaceFillRect gSurface Nothing $
      -- setting color with R-G-B-A
      V4 maxBound maxBound minBound maxBound

   -- update the surface for a specific window
   SDL.updateWindowSurface window

   -- wait two seconds :
   -- https://hackage.haskell.org/package/sdl2-2.1.2.1/docs/SDL-Time.html#v:delay
   -- Users are generally recommended to use threadDelay instead, to take
   -- advantage of the abilities of the Haskell runtime.
   threadDelay 2000000

   -- destroy window
   SDL.destroyWindow window
   SDL.quit
