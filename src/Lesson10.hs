{- applyinhg Key and copy texture without scaling -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson10 where
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
import Control.Exception (catch)
--
import qualified Config
--
import System.Exit (die)
--

-- definition of LTextureexture
data LTexture = LTexture {getTx :: SDL.Texture, getWH :: (V2 CInt)}
--
class Renderable a where
   renderQuad :: a -> CInt -> CInt -> Maybe (SDL.Rectangle CInt)
   render :: SDL.Renderer -> a -> CInt -> CInt -> IO ()
   free :: a ->  IO ()
--
instance Renderable LTexture where
   renderQuad ltx x y =
      Just $ SDL.Rectangle (P $ V2 x y) $ getWH ltx
   render rdr ltx x y = do
      SDL.copy rdr (getTx ltx) Nothing (renderQuad ltx x y)
   free ltx = SDL.destroyTexture (getTx ltx)

-- definition of key
yellowKey :: Maybe (V4 Word8)
yellowKey = Just $ V4 maxBound maxBound 0 maxBound

-- definition of loading function
loadFromFile :: SDL.Renderer -> FilePath -> IO LTexture
loadFromFile rdr path = do
   tempSf <- SDL.loadBMP path
   wh <- SDL.surfaceDimensions tempSf
   -- ************ --
   SDL.surfaceColorKey tempSf SDL.$= yellowKey
   -- ************ --
   tx <- SDL.createTextureFromSurface rdr tempSf
   SDL.freeSurface tempSf
   return (LTexture tx wh)
   -- note: in lazyfoo's tutorial, the function mapRGB is called
   -- however, in sdl2 haskell binding,
   -- mapRGB is not needed and thus flagged as deprecated

--
lesson10 :: IO ()
lesson10 = do
   -- initialize SDL
   run (SDL.initialize [SDL.InitVideo])
       "SDL could not initialize!"

   -- create window
   window <- run (SDL.createWindow "Lesson10" Config.winConfig)
                 "Window could not be created!"
   SDL.HintRenderScaleQuality SDL.$= SDL.ScaleLinear
   renderer <- SDL.createRenderer window (-1) Config.rdrConfig
   SDL.rendererDrawColor renderer SDL.$=
      V4 maxBound maxBound minBound maxBound

   imgBg <- loadFromFile renderer "./img/10/bg.bmp"
   imgHumanish <- loadFromFile renderer "./img/10/humanish.bmp"

   let
      loop = do
         events <- SDL.pollEvents
         let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
         -- *** beginning of drawing region ***
         SDL.rendererDrawColor renderer SDL.$=
            V4 minBound minBound maxBound maxBound
         SDL.clear renderer
         -- render with our own function
         render renderer imgBg 0 0
         render renderer imgHumanish 240 190
         --
         SDL.present renderer
         -- *** end of drawing region ***
         unless quit loop
   loop
   free imgBg
   free imgHumanish
   SDL.destroyRenderer renderer
   SDL.destroyWindow window
   SDL.quit

-- if something wrong then exit the program
run :: IO a -> String -> IO a
run exec errMessage =
    catch exec
          (\e -> do let err = show (e :: SDL.SDLException)
                    die (errMessage ++ "\nSDL_Error: "++ err))
