{- applying viewport -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson09 where
--
import qualified SDL
--
import Data.Word (Word8(..))
import Linear.Affine (Point(..))
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Foreign.C.Types (CInt)
--
import Control.Monad (unless,forM_)
import Control.Applicative ((<*))
import Control.Exception (catch)
--
import qualified Config
--
import System.Exit (die)
--

-- define viewports
viewportLU, viewportRU, viewportD :: Maybe (SDL.Rectangle CInt)
viewportLU = Just $ SDL.Rectangle
   (P $ V2 0 0)
   (fmap (`div` 2) Config.winV2)
viewportRU = Just $ SDL.Rectangle
   (P $ V2 (Config.winW `div` 2) 0)
   (fmap (`div` 2) Config.winV2)
viewportD  = Just $ SDL.Rectangle
   (P $ V2 0 (Config.winH `div` 2))
   (V2 Config.winW (Config.winH `div` 2))
--
lesson09 :: IO ()
lesson09 = do
   -- initialize SDL
   run (SDL.initialize [SDL.InitVideo])
       "SDL could not initialize!"

   -- create window
   window <- run (SDL.createWindow "Lesson09" Config.winConfig)
                 "Window could not be created!"
   SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
   renderer <- SDL.createRenderer window (-1) Config.rdrConfig
   SDL.rendererDrawColor renderer SDL.$=
      V4 maxBound maxBound minBound maxBound

   -- load image files
   imgTxLU <- loadImgAsTexture renderer "./img/09/left.bmp"
   imgTxRU <- loadImgAsTexture renderer "./img/09/right.bmp"
   imgTxD  <- loadImgAsTexture renderer "./img/09/down.bmp"

   let
      loop = do
         events <- SDL.pollEvents
         let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
         -- *** beginning of drawing region ***
         SDL.rendererDrawColor renderer SDL.$=
            V4 minBound minBound maxBound maxBound
         SDL.clear renderer
         --
         SDL.rendererViewport renderer SDL.$= viewportLU
         SDL.copy renderer imgTxLU Nothing Nothing
         --
         SDL.rendererViewport renderer SDL.$= viewportRU
         SDL.copy renderer imgTxRU Nothing Nothing
         --
         SDL.rendererViewport renderer SDL.$= viewportD
         SDL.copy renderer imgTxD Nothing Nothing
         --
         SDL.present renderer
         -- *** end of drawing region ***
         unless quit loop
   loop

   SDL.destroyTexture imgTxD
   SDL.destroyTexture imgTxRU
   SDL.destroyTexture imgTxLU
   SDL.destroyWindow window
   SDL.destroyRenderer renderer
   SDL.quit
--
loadImgAsTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadImgAsTexture rdr path = do
   tempSf <- SDL.loadBMP path
   SDL.createTextureFromSurface rdr tempSf <* SDL.freeSurface tempSf

-- if something wrong then exit the program
run :: IO a -> String -> IO a
run exec errMessage =
    catch exec
          (\e -> do let err = show (e :: SDL.SDLException)
                    die (errMessage ++ "\nSDL_Error: "++ err))
