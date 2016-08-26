{- blit optmized image with scaling -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson05 where
--
import qualified SDL
import Linear.V2 (V2(V2))
import Linear.Affine(Point(P))
--
import Control.Monad (unless)
import Control.Applicative ((<*))
import Control.Exception (catch,IOException)
--
import qualified Config
--
import System.Exit (die)


-- In fact, SDL converts color mode in every blitting if
-- the color mode of source surface doesn't match
-- the color mode of target surface.
-- To avoid those converting, a simple way is to
-- align their color mode whenever we load an image.
optLoadBMPwith :: SDL.Surface -> FilePath -> IO SDL.Surface
optLoadBMPwith originSf path = do
   imgSf <- SDL.loadBMP path
   -- get the color mode of given surface
   spf <- SDL.surfaceFormat originSf
   -- align the color mode of image surface
   SDL.convertSurface imgSf spf
      <* SDL.freeSurface imgSf
   -- equals to the following lines
   -- optSf <- SDL.convertSurface imgSf spf
   -- SDL.freeSurface imgSf
   -- return optSf
--
lesson05 :: IO ()
lesson05 = do
   -- initialize SDL
   run (SDL.initialize [SDL.InitVideo])
       "SDL could not initialize!"

   -- create window
   window <- run (SDL.createWindow "Lesson05" Config.winConfig)
                 "Window could not be created!"

   gSurface <- SDL.getWindowSurface window
   sf <- run (optLoadBMPwith gSurface "./img/05/up.bmp")
             "Failed to load stretching image"
   let
      loop = do
         events <- SDL.pollEvents
         let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
         -- blit with given scaling setup
         -- Nothing for default setup - blitting with fully filling
         SDL.surfaceBlitScaled sf Nothing gSurface
            $ Just $ SDL.Rectangle (P (V2 0 0)) (V2 200 200)
         SDL.updateWindowSurface window
         unless quit loop
   loop

   -- Free resources and close SDL
   SDL.freeSurface sf
   SDL.destroyWindow window
   SDL.quit

-- .

-- if something wrong then exit the program
run :: IO a -> String -> IO a
run exec errMessage =
    catch exec
          (\e -> do let err = show (e :: IOException)
                    die (errMessage ++ " SDL_Error: "++ err))
