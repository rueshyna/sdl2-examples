{- draw sprite with image clipping -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson17 where
--
import qualified SDL
--
import Data.Word (Word8(..))
import Linear.Affine (Point(..))
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Foreign.C.Types (CInt)
--
import Control.Monad (unless)
--
import qualified Config
--

-- setup xywh for all clips
gSpriteClips :: LButtonSprite -> SDL.Rectangle CInt
gSpriteClips Out        = SDL.Rectangle (P $ V2   0   0) (V2 100 100)  -- LU
gSpriteClips OverMotion = SDL.Rectangle (P $ V2 100   0) (V2 100 100)  -- RU
gSpriteClips Down       = SDL.Rectangle (P $ V2   0 100) (V2 100 100)  -- LD
gSpriteClips Up         = SDL.Rectangle (P $ V2 100 100) (V2 100 100)  -- RD

getClipsW :: SDL.Rectangle CInt -> CInt
getClipsW xs = let (SDL.Rectangle _ (V2 w _)) = xs in w
getClipsH :: SDL.Rectangle CInt -> CInt
getClipsH xs = let (SDL.Rectangle _ (V2 _ h)) = xs in h

gButtons :: [LButton]
gButtons =
    [ LButton (V2 0 0) (V2 100 100) Out
    , LButton (V2 100 0) (V2 100 100) Out
    , LButton (V2 0 100) (V2 100 100) Out
    , LButton (V2 100 100) (V2 100 100) Out
    ]

-- definition of LTexture
data LTexture = LTexture {getTx :: SDL.Texture, getTextureWH :: V2 CInt}

-- definition of GButton
data LButton = LButton
             { getPos :: V2 CInt
             , getButtonWH  :: V2 CInt
             , imgCurrentIndex :: LButtonSprite
             }

-- definition of LButtonSprite
data LButtonSprite = Out
                   | OverMotion
                   | Down
                   | Up

changeIdx :: LButton -> LButtonSprite -> LButton
changeIdx (LButton p s sprite) = LButton p s

--
class Renderable a where
   render :: SDL.Renderer -> a -> SDL.Rectangle CInt -> V2 CInt -> IO ()
   free :: a ->  IO ()
--
instance Renderable LTexture where
   render rdr ltx xywh@(SDL.Rectangle _ (V2 w h)) xy =
      SDL.copy rdr (getTx ltx) (Just xywh) (Just $ SDL.Rectangle (P xy) (V2 w h))
   free ltx = SDL.destroyTexture (getTx ltx)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

triggerEvent :: Maybe SDL.EventPayload -> LButton -> LButton
triggerEvent (Just (SDL.MouseMotionEvent m)) lbt = changeIdx lbt OverMotion
triggerEvent (Just (SDL.MouseButtonEvent m)) lbt =
    case input of
      SDL.Released -> changeIdx lbt Up
      SDL.Pressed  -> changeIdx lbt Down
      _        -> lbt
    where input = SDL.mouseButtonEventMotion m
triggerEvent _ lbt = lbt

inside ::  Point V2 CInt -> LButton -> Either LButton LButton
inside ( P (V2 cx cy)) lb
   | cx < x     = Right lb
   | cx > x + w = Right lb
   | cy < y     = Right lb
   | cy > y + h = Right lb
   | otherwise  =  Left lb
   where V2 x y  = getPos lb
         V2 w h  = getButtonWH lb

inside' ::  Point V2 CInt -> LButton -> Maybe (Point V2 CInt)
inside' p@(P (V2 cx cy)) lb
   | cx < x     = Nothing
   | cx > x + w = Nothing
   | cy < y     = Nothing
   | cy > y + h = Nothing
   | otherwise  =  Just p
   where V2 x y  = getPos lb
         V2 w h  = getButtonWH lb

-- definition of loading function
loadFromFile :: SDL.Renderer -> FilePath -> IO LTexture
loadFromFile rdr path = do
   tempSf <- SDL.loadBMP path
   wh <- SDL.surfaceDimensions tempSf
   -- ************ --
   --SDL.surfaceColorKey tempSf SDL.$= Just (V4 223 113 38 maxBound)
   SDL.surfaceColorKey tempSf SDL.$= Just (V4 0 113 38 maxBound)
   tx <- SDL.createTextureFromSurface rdr tempSf
   SDL.freeSurface tempSf
   return (LTexture tx wh)

--
lesson17 :: IO ()
lesson17 = do
   -- initialize SDL
   SDL.initialize [SDL.InitVideo]

   -- create window
   window <- SDL.createWindow "Lesson17" Config.winConfig
   renderer <- SDL.createRenderer window (-1) Config.rdrConfig
   SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
   SDL.rendererDrawColor renderer SDL.$=
       V4 minBound minBound maxBound maxBound

   gSpriteSheetTexture <- loadFromFile renderer "./img/17/sprite.bmp"

   let
      loop gButtons = do
         events <- SDL.pollEvents
         let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
         -- *** beginning of drawing region ***
         SDL.rendererDrawColor renderer SDL.$=
            V4 minBound minBound maxBound maxBound
         SDL.clear renderer
         -- render with our own function
         --
         mouseLocation <- SDL.getAbsoluteMouseLocation
         let eitherButtons = map (inside mouseLocation) gButtons
         let effectiveEvent = fmap SDL.eventPayload (head' events)
         let gButtons' = map (either (triggerEvent effectiveEvent) (`changeIdx` Out)) eitherButtons
         render renderer gSpriteSheetTexture (gSpriteClips $ imgCurrentIndex (gButtons'!!0)) $ V2 0 0
         render renderer gSpriteSheetTexture (gSpriteClips $ imgCurrentIndex (gButtons'!!1)) $ V2 100 0
         render renderer gSpriteSheetTexture (gSpriteClips $ imgCurrentIndex (gButtons'!!2)) $ V2 0 100
         render renderer gSpriteSheetTexture (gSpriteClips $ imgCurrentIndex (gButtons'!!3)) $ V2 100 100
         --
         SDL.present renderer
         -- *** end of drawing region ***
         unless quit $ loop gButtons'
   loop gButtons
   free gSpriteSheetTexture
   SDL.destroyRenderer renderer
   SDL.destroyWindow window
   SDL.quit
