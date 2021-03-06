{- draw sprite with image clipping -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson14 where
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
--
import qualified Config
--

walkingAnimationFrames :: Integer
walkingAnimationFrames = 4

-- setup xywh for all clips
gSpriteClips :: [SDL.Rectangle CInt]
gSpriteClips =
   [ rect   0   0 100 100  -- LU
   , rect 100   0 100 100  -- RU
   , rect   0 100 100 100  -- LD
   , rect 100 100 100 100] -- RD
   where rect x y w h = SDL.Rectangle (P$V2 x y) (V2 w h)

-- definition of LTexture
data LTexture = LTexture {getTx :: SDL.Texture, getWH :: V2 CInt}
--
class Renderable a where
   render :: SDL.Renderer -> a -> SDL.Rectangle CInt -> V2 CInt -> IO ()
   free :: a ->  IO ()
--
instance Renderable LTexture where
   render rdr ltx xywh@(SDL.Rectangle _ (V2 w h)) xy = do
      SDL.copy rdr (getTx ltx) (Just xywh) (Just $ SDL.Rectangle (P xy) (V2 w h))
   free ltx = SDL.destroyTexture (getTx ltx)

-- definition of loading function
loadFromFile :: SDL.Renderer -> FilePath -> IO LTexture
loadFromFile rdr path = do
   tempSf <- SDL.loadBMP path
   wh <- SDL.surfaceDimensions tempSf
   -- ************ --
   SDL.surfaceColorKey tempSf SDL.$= Just (V4 maxBound maxBound maxBound maxBound)
   tx <- SDL.createTextureFromSurface rdr tempSf
   SDL.freeSurface tempSf
   return (LTexture tx wh)

--
lesson14 :: IO ()
lesson14 = do
   -- initialize SDL
   SDL.initialize [SDL.InitVideo]

   -- create window
   window <- SDL.createWindow "Lesson14" Config.winConfig
   renderer <- SDL.createRenderer window (-1) Config.rdrConfig
   SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
   SDL.rendererDrawColor renderer SDL.$=
      V4 maxBound maxBound minBound maxBound

   gSpriteSheetTexture <- loadFromFile renderer "./img/14/sprite.bmp"

   let
       loop frame = do
         events <- SDL.pollEvents
         let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
         -- *** beginning of drawing region ***

         SDL.rendererDrawColor renderer SDL.$= V4 maxBound maxBound maxBound maxBound
         SDL.clear renderer
         -- render with our own function
         --
         render renderer gSpriteSheetTexture (gSpriteClips !! floor(toRational frame / 4)) -- LU
            $ V2 0 0

         let countFrame f
                | floor(toRational f / 4) >= walkingAnimationFrames = 0
                | otherwise = f

         let frame' = countFrame $ frame + 1

         SDL.present renderer
         -- *** end of drawing region ***
         unless quit (loop frame')
   loop 0

   free gSpriteSheetTexture
   SDL.destroyRenderer renderer
   SDL.destroyWindow window
   SDL.quit
