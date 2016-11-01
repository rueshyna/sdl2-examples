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
--
import qualified Config
--
import Utility
--


-- In fact, SDL converts color mode in every blitting if
-- the color mode of source surface doesn't match
-- the color mode of target surface.
-- To avoid those converting, a simple way is to
-- align their color mode whenever we load an image.
optLoadBmpPic :: SDL.Surface -> FilePath -> IO SDL.Surface
optLoadBmpPic originSf path = do
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
lesson05
    = (^.^) sdlInit ()                 -- initialize SDL
    $ \() -> (^.^) window "Lesson05"   -- create window
    $ \w -> (^.^) surface w            -- get surface from given window
    $ \s -> (^.^) (uncurry optLoadBmpPic, SDL.freeSurface) (s, "./img/hellowworld.bmp")
    $ \o -> do

      let
         loop = do
            events <- SDL.pollEvents
            let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events
            -- blit with given scaling setup
            -- Nothing for default setup - blitting with fully filling
            SDL.surfaceBlitScaled o Nothing s
               $ Just $ SDL.Rectangle (P (V2 0 0)) (V2 400 300)
            SDL.updateWindowSurface w
            unless quit loop
      loop
