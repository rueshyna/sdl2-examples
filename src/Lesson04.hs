{- Keyboard events handling -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson04 where
--
import qualified SDL
import Linear.V4 (V4(..))
--
import Data.Monoid
import Data.Maybe
--
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
--
import Utility
--

lesson04 :: IO ()
lesson04
    = (^.^) sdlInit ()                 -- initialize SDL
    $ \() -> (^.^) window "Lesson04"   -- create window
    $ \w -> (^.^) surface w            -- get surface from given window
    -- load image file as a surface
    $ \s -> (^.^) loadBmpPic "./img/press.bmp"  -- default image
    $ \p -> (^.^) loadBmpPic "./img/up.bmp"     -- up image
    $ \u -> (^.^) loadBmpPic "./img/down.bmp"   -- down impage
    $ \d -> (^.^) loadBmpPic "./img/left.bmp"   -- left impage
    $ \l -> (^.^) loadBmpPic "./img/right.bmp"  -- right image
    $ \r -> do

      -- function to convert a event into a surface
      let e2s = Last.(eventToSurface u d l r p).SDL.eventPayload

      -- define main loop with extra parameter: current blitted surface
      let loop = \prevSurface -> do
            events <- SDL.pollEvents
            let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
            let newSurface =
                  fromMaybe prevSurface
                  -- select the last surface (or nothing)
                  $ getLast
                  -- convert all events into a list of Maybe surface
                  $ foldMap e2s events
            SDL.surfaceBlit newSurface Nothing s Nothing
            SDL.updateWindowSurface w
            unless quit $ loop newSurface

      -- exec main loop
      loop p

-- to decide a surface to blit from given event info.
eventToSurface :: SDL.Surface -- for up-event
               -> SDL.Surface -- for down-event
               -> SDL.Surface -- for left-event
               -> SDL.Surface -- for right-event
               -> SDL.Surface -- for default
               -> SDL.EventPayload
               -> Maybe SDL.Surface
eventToSurface picUp
               picDown
               picLeft
               picRight
               picDefault
               (SDL.KeyboardEvent ked) =
   case (SDL.keysymKeycode $ SDL.keyboardEventKeysym ked) of
      SDL.KeycodeUp     -> Just picUp
      SDL.KeycodeDown   -> Just picDown
      SDL.KeycodeLeft   -> Just picLeft
      SDL.KeycodeRight  -> Just picRight
      SDL.KeycodeEscape -> Just picDefault
      otherwise        -> Nothing
-- if input event is not KeyboardEvent then return Nothing
eventToSurface _ _ _ _ _ _      = Nothing
