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
import Control.Monad (unless)
--
import Config
import PixelPen
--

lesson04 :: IO ()
lesson04
    = (^.^) sdlInitVideo ()                     -- initialize SDL
    $ \() -> (^.^) defaultWindow "Lesson04"     -- create window
    $ \w -> (^.^) surface w                     -- get surface from given window
    -- load image file as a surface
    $ \s -> (^.^) loadBmpPic "./img/press.bmp"  -- default image
    $ \p -> (^.^) loadBmpPic "./img/up.bmp"     -- up image
    $ \u -> (^.^) loadBmpPic "./img/down.bmp"   -- down impage
    $ \d -> (^.^) loadBmpPic "./img/left.bmp"   -- left impage
    $ \l -> (^.^) loadBmpPic "./img/right.bmp"  -- right image
    $ \r -> update_ p $ eventHandle_ checkDefaultQuit
      (\f t -> do
               let newS = eventToSurface u d l r p $ SDL.eventPayload f
               return $ fromMaybe p newS
      )
      (\f t -> do
               SDL.surfaceBlit t Nothing s Nothing
               SDL.updateWindowSurface w
      )
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
