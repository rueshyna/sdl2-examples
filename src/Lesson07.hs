{- Using textures instead of surfaces -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson07 where
--
import qualified SDL
import Linear.V4 (V4(..))
--
import Control.Monad (unless,when)
import Control.Exception (catch)
--
import qualified Config
--
import System.Exit (die)

lesson07 :: IO ()
lesson07 = do
   -- initialize SDL
   run (SDL.initialize [SDL.InitVideo])
       "SDL could not initialize!"

   -- create window
   window <- run (SDL.createWindow "Lesson07" Config.winConfig)
                 "Window could not be created!"
   -- using Hint, comment out for seeing the effects
   -- reference: https://en.wikipedia.org/wiki/Image_scaling#Scaling_methods
   -- ***************
   SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
   -- ***************

   renderer <- run (SDL.createRenderer window (-1) Config.rdrConfig)
                   "Renderer could not be created!"

   -- set a color for renderer
   SDL.rendererDrawColor renderer
      SDL.$= V4 minBound minBound maxBound maxBound

   -- load image into main memory (as a surface)
   imgSf <- SDL.loadBMP "./img/07/Potion.bmp"
   -- translate a surface to a texture
   -- i.e. load image into video memory
   imgTx <- SDL.createTextureFromSurface renderer imgSf
   SDL.freeSurface imgSf

   let
      loop = do
         events <- SDL.pollEvents
         let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
         -- clear(i.e. fill) renderer with the color we set
         SDL.clear renderer
         -- copy(blit) image texture onto renderer
         SDL.copy renderer imgTx Nothing Nothing
         -- A renderer in SDL is basically a buffer
         -- the present function forces a renderer to flush
         SDL.present renderer
         --
         unless quit loop
   loop
   -- releasing resources
   SDL.destroyWindow window
   SDL.destroyRenderer renderer
   SDL.destroyTexture imgTx
   SDL.quit

-- if something wrong then exit the program
run :: IO a -> String -> IO a
run exec errMessage =
    catch exec
          (\e -> do let err = show (e :: SDL.SDLException)
                    die (errMessage ++ "\nSDL_Error: "++ err))
