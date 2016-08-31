{- draw sprite with image clipping -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson12 where
--
import qualified SDL
--
import Data.Monoid
import Data.Word (Word8(..))
import Linear.Affine (Point(..))
import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))
import Foreign.C.Types (CInt)
--
import Control.Monad (unless)
import Control.Exception (catch)
--
import qualified Config
--
import System.Exit (die)
--

-- definition of LTexture
data LTexture = LTexture {getTx :: SDL.Texture, getWH :: V2 CInt}
--
class Renderable a where
   renderQuad :: a -> CInt -> CInt -> Maybe (SDL.Rectangle CInt)
   render :: SDL.Renderer -> a -> CInt -> CInt -> IO ()
   free :: a ->  IO ()
--
instance Renderable LTexture where
   renderQuad ltx x y =
      Just $ SDL.Rectangle (P $ V2 x y) $ getWH ltx
   render rdr ltx x y =
      SDL.copy rdr (getTx ltx) Nothing (renderQuad ltx x y)
   free ltx = SDL.destroyTexture (getTx ltx)

-- definition of loading function
loadFromFile :: SDL.Renderer -> FilePath -> IO LTexture
loadFromFile rdr path = do
   tempSf <- SDL.loadBMP path
   wh <- SDL.surfaceDimensions tempSf
   -- ************ --
   SDL.surfaceColorKey tempSf SDL.$= Just (V4 223 113 38 maxBound)
   tx <- SDL.createTextureFromSurface rdr tempSf
   SDL.freeSurface tempSf
   return (LTexture tx wh)

--
lesson12 :: IO ()
lesson12 = do
   -- initialize SDL
   run (SDL.initialize [SDL.InitVideo])
       "SDL could not initialize!"

   -- create window
   window <- run (SDL.createWindow "Lesson11" Config.winConfig)
                 "Window could not be created!"
   renderer <- SDL.createRenderer window (-1) Config.rdrConfig
   SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
   SDL.rendererDrawColor renderer SDL.$=
      V4 maxBound maxBound minBound maxBound

   gSpriteSheetTexture <- loadFromFile renderer "./img/12/sprite.bmp"

   let
      loop (r, g, b) = do
         events <- SDL.pollEvents
         let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

         let checkKeySym (SDL.KeyboardEvent keysym:xs) = keysym : checkKeySym xs
             checkKeySym (sym:xs) = checkKeySym xs
             checkKeySym [] = []

         let pressedKey =  map (SDL.keysymKeycode . SDL.keyboardEventKeysym) $ checkKeySym $ map SDL.eventPayload events

         let adjustColor SDL.KeycodeQ = (32, 0, 0)
             adjustColor SDL.KeycodeW = (0, 32, 0)
             adjustColor SDL.KeycodeE = (0, 0, 32)
             adjustColor SDL.KeycodeA = (-32, 0, 0)
             adjustColor SDL.KeycodeS = (0, -32, 0)
             adjustColor SDL.KeycodeD = (0, 0, -32)
             adjustColor _ = (0,0,0)

         -- *** beginning of drawing region ***
         let (r', g', b') = foldr (\(a1,b1,c1) (a2,b2,c2) -> (a1+a2,b1+b2,c1+c2)) (r, g, b) $ map adjustColor pressedKey

         SDL.rendererDrawColor renderer SDL.$= V4 maxBound maxBound maxBound maxBound
         SDL.clear renderer
         -- render with our own function

         SDL.textureColorMod (getTx gSpriteSheetTexture) SDL.$= V3 r' g' b'
         render renderer gSpriteSheetTexture 0 0

         SDL.present renderer
         -- *** end of drawing region ***
         unless quit (loop (r', g', b'))
   loop (255, 255, 255)

   free gSpriteSheetTexture
   SDL.destroyRenderer renderer
   SDL.destroyWindow window
   SDL.quit

--
run :: IO a -> String -> IO a
run exec errMessage =
    catch exec
          (\e -> do let err = show (e :: SDL.SDLException)
                    die (errMessage ++ "\nSDL_Error: "++ err))
