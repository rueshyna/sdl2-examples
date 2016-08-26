{-  Load and display image via surface -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson02 where
--
import qualified SDL
import Linear.V4 (V4(..))
--
import Control.Concurrent (threadDelay)
import Control.Exception (catch)
--
import qualified Config
--
import System.Exit (die)

lesson02 :: IO ()
lesson02 = do
   -- initialize SDL
   run (SDL.initialize [SDL.InitVideo])
       "SDL could not initialize!"

   -- create window
   window <- run (SDL.createWindow "Lesson02" Config.winConfig)
                 "Window could not be created!"

   -- get surface from given window
   gSurface <- SDL.getWindowSurface window

   -- load image file as a surface
   pictureS <- run (SDL.loadBMP "./img/02/Broom.bmp")
                   "Unable to load image!"

   -- blit(copy/show) image surface onto window surface
   SDL.surfaceBlit pictureS Nothing gSurface Nothing

   -- update the surface
   SDL.updateWindowSurface window

   -- wait two seconds
   threadDelay 2000000

   -- free surface
   SDL.freeSurface pictureS

   -- destroy window
   SDL.destroyWindow window

   -- quit SDL subsystems
   SDL.quit

-- if something wrong then exit the program
run :: IO a -> String -> IO a
run exec errMessage =
    catch exec
          (\e -> do let err = show (e :: SDL.SDLException)
                    die (errMessage ++ "\nSDL_Error: "++ err))
