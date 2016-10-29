{- Keyboard events handling -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson18 where
--
import qualified SDL
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Linear.Affine (Point(..))
import Foreign.C.Types (CInt)
--
import Data.Monoid
import Data.Maybe
--
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
--
import qualified Config

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
   SDL.surfaceColorKey tempSf SDL.$= Just (V4 maxBound maxBound maxBound maxBound)
   tx <- SDL.createTextureFromSurface rdr tempSf
   SDL.freeSurface tempSf
   return (LTexture tx wh)

head' :: [a] -> Maybe a
head' [] = Nothing
head' (x:xs) = Just x

lesson18 :: IO ()
lesson18 = do
   -- initialize SDL
   SDL.initialize [SDL.InitVideo]

   -- create window
   window <- SDL.createWindow "Lesson18" Config.winConfig
   renderer <- SDL.createRenderer window (-1) Config.rdrConfig
   SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
   SDL.rendererDrawColor renderer SDL.$=
       V4 minBound minBound maxBound maxBound

   --
   picDefault <- loadFromFile renderer "./img/18/press.bmp"
   picUp      <- loadFromFile renderer "./img/18/up.bmp"
   picDown    <- loadFromFile renderer "./img/18/down.bmp"
   picLeft    <- loadFromFile renderer "./img/18/left.bmp"
   picRight   <- loadFromFile renderer "./img/18/right.bmp"

   let state =
             [ (picUp      ,SDL.ScancodeUp)
             , (picDown    ,SDL.ScancodeDown)
             , (picLeft    ,SDL.ScancodeLeft)
             , (picRight   ,SDL.ScancodeRight)
             ]

   -- define main loop with extra parameter: current blitted surface
   let loop texture = do
         events <- SDL.pollEvents
         let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
         SDL.rendererDrawColor renderer SDL.$= V4 maxBound maxBound maxBound maxBound
         SDL.clear renderer
         currentKeyStates <- SDL.getKeyboardState
         let triggerStates = filter (currentKeyStates . snd) state
         let newPic = maybe picDefault fst $ head' triggerStates

         render renderer newPic 0 0

         SDL.present renderer
         -- *** end of drawing region ***
         unless quit $ loop texture

   -- exec main loop
   loop picDefault

   -- release resources
   SDL.destroyWindow window
   free picDefault
   free picUp
   free picDown
   free picLeft
   free picRight
   SDL.quit
