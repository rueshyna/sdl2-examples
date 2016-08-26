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
import Control.Exception (catch)
--
import qualified Config
--
import System.Exit (die)

lesson04 :: IO ()
lesson04 = do
   -- initialize SDL
   run (SDL.initialize [SDL.InitVideo])
       "SDL could not initialize!"

   -- create window
   window <- run (SDL.createWindow "Lesson04" Config.winConfig)
                 "Window could not be created!"

   gSurface <- SDL.getWindowSurface window

   --
   picDefault <- run (SDL.loadBMP "./img/04/press.bmp") "failed to load up image!"
   picUp      <- run (SDL.loadBMP "./img/04/up.bmp") "failed to load up image!"
   picDown    <- run (SDL.loadBMP "./img/04/down.bmp") "failed to load up image!"
   picLeft    <- run (SDL.loadBMP "./img/04/left.bmp") "failed to load up image!"
   picRight   <- run (SDL.loadBMP "./img/04/right.bmp") "failed to load up image!"

   -- function to convert a event into a surface
   let e2s = Last.(eventToSurface picUp picDown picLeft picRight picDefault).SDL.eventPayload

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
         SDL.surfaceBlit newSurface Nothing gSurface Nothing
         SDL.updateWindowSurface window
         unless quit $ loop newSurface

   -- exec main loop
   loop picDefault

   -- release resources
   SDL.destroyWindow window
   SDL.freeSurface picDefault
   SDL.freeSurface picUp
   SDL.freeSurface picDown
   SDL.freeSurface picLeft
   SDL.freeSurface picRight
   SDL.quit

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

-- if something wrong then exit the program
run :: IO a -> String -> IO a
run exec errMessage =
    catch exec
          (\e -> do let err = show (e :: SDL.SDLException)
                    die (errMessage ++ "\nSDL_Error: "++ err))
