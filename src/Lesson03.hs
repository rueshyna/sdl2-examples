{- Basic events handling -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson03 where
--
import qualified SDL
import Linear.V4 (V4(..))
--
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Exception (catch,IOException)
--
import qualified Config
--
import System.Exit (die)

lesson03 :: IO ()
lesson03 = do
   -- initialize SDL
   run (SDL.initialize [SDL.InitVideo])
       "SDL could not initialize!"

   -- create window
   window <- run (SDL.createWindow "Lesson03" Config.winConfig)
                 "Window could not be created!"

   -- get surface from given window
   gSurface <- SDL.getWindowSurface window

   -- load image file as a surface
   pictureS <- run (SDL.loadBMP "./img/03/Broom.bmp")
                   "Unable to load image!"

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

-- if something wrong then exit the program
run :: IO a -> String -> IO a
run exec errMessage =
    catch exec
          (\e -> do let err = show (e :: IOException)
                    die (errMessage ++ " SDL_Error: "++ err))
