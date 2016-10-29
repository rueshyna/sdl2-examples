{- draw sprite with image clipping -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson15 where
--
import qualified SDL
--
import qualified Data.Boolean as DB
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

-- definition of LTexture
data LTexture = LTexture {getTx :: SDL.Texture, getWH :: V2 CInt}
--
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

{-instance DB.Boolean (V2 Bool) where-}
    {-(&&) (V2 x1 y1) (V2 x2 y2) = V2 (x1 && x2) (y1 && y2)-}
    {-(||) (V2 x1 y1) (V2 x2 y2) = V2 (x1 || x2) (y1 || y2)-}
    {-xor (V2 x1 y1) (V2 x2 y2) = V2 (DB.xor x1 x2) (DB.xor y1 y2)-}
    {-not (V2 x1 y1) = V2 (not x1) (not y1)-}

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
lesson15 :: IO ()
lesson15 = do
   -- initialize SDL
   SDL.initialize [SDL.InitVideo]

   -- create window
   window <- SDL.createWindow "Lesson15" Config.winConfig
   renderer <- SDL.createRenderer window (-1) Config.rdrConfig
   SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
   SDL.rendererDrawColor renderer SDL.$=
      V4 maxBound maxBound minBound maxBound

   gSpriteSheetTexture <- loadFromFile renderer "./img/15/right.bmp"

   let
       loop (degree, flip) = do
         events <- SDL.pollEvents
         let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
         -- *** beginning of drawing region ***

         SDL.rendererDrawColor renderer SDL.$= V4 maxBound maxBound maxBound maxBound
         SDL.clear renderer

         let checkKeySym (SDL.KeyboardEvent keysym:xs) = keysym : checkKeySym xs
             checkKeySym (sym:xs) = checkKeySym xs
             checkKeySym [] = []

         let pressedKey =  map (SDL.keysymKeycode . SDL.keyboardEventKeysym) $ checkKeySym $ map SDL.eventPayload events

         let adjustDegree SDL.KeycodeA = (60, V2 False False)
             adjustDegree SDL.KeycodeD = (-60, V2 False False)
             adjustDegree SDL.KeycodeQ = (0, V2 True False)
             adjustDegree SDL.KeycodeW = (0, V2 True True)
             adjustDegree SDL.KeycodeE = (0, V2 True False)
             adjustDegree _ = (0, V2 False False)

         -- let (degree', flip') = foldr (\(degree1, (V2 x1 y1)) (degree2, (V2 x2 y2)) -> (degree1 + degree2, (V2 (x1 || x2) (y1 || y2)))) (degree, flip) $ map adjustDegree pressedKey
         let (degree', flip') = foldr (\(degree1, flip1) (degree2, flip2) -> (degree1 + degree2, flip1 DB.|| flip2)) (degree, flip) $ map adjustDegree pressedKey
         -- render with our own function
         --
         SDL.rendererDrawColor renderer SDL.$= V4 maxBound maxBound minBound maxBound
         SDL.clear renderer

         SDL.copyEx renderer (getTx gSpriteSheetTexture) Nothing Nothing degree' (Just (P $ V2 320 400)) flip'

         SDL.present renderer
         -- *** end of drawing region ***
         unless quit (loop (degree', flip'))
   loop (0, pure False)

   free gSpriteSheetTexture
   SDL.destroyRenderer renderer
   SDL.destroyWindow window
   SDL.quit
