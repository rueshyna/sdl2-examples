{- Keyboard events handling -}
{-# LANGUAGE OverloadedStrings #-}
module Lesson04 where
--
import qualified SDL
--
import Data.Monoid
import Data.Maybe
import Data.Foldable
import Control.Applicative
import Control.Monad.IO.Class (MonadIO)

--
import PixelPen
--

data Input = IDefault
           | IDown
           | IUp
           | IRight
           | ILeft
           deriving (Show, Eq, Ord, Enum)

data Mapping a = M Input a
type MapKeycode = Mapping SDL.Keycode
type MapPicPath = Mapping String
type MapPic = Mapping SDL.Surface

data State a = S [a] a [a]

mapPicPath :: State (MapPicPath)
mapPicPath = S [] ( M IDefault "./img/press.bmp")
                [ M IDown    "./img/down.bmp"
                , M IUp      "./img/up.bmp"
                , M IRight   "./img/right.bmp"
                , M ILeft    "./img/left.bmp"
                ]

mapping :: [ MapKeycode ]
mapping = [ M IDefault SDL.KeycodeEscape
          , M IDown    SDL.KeycodeDown
          , M IUp      SDL.KeycodeUp
          , M IRight   SDL.KeycodeRight
          , M ILeft    SDL.KeycodeLeft
          ]

toInput :: [ MapKeycode ] -> SDL.Keycode -> Maybe Input
toInput [] _ = Nothing
toInput (M i k:xs) mk
    | k == mk = Just i
    | k /= mk = toInput xs mk

class Moveable m where
    left :: m a -> m a
    right :: m a-> m a

instance Moveable State where
  left  (S [] c rs) = S [] c rs
  left  (S (l:ls) c rs) = S ls l (c:rs)

  right (S ls c []) = S ls c []
  right (S ls c (r:rs)) = S (c:ls) r rs

instance Functor State where
  fmap f (S xs y zs) = S (map f xs) (f y) (map f zs)

instance Foldable State where
  foldr f w (S xs y zs) = foldr f (f y (foldr f w zs)) $ reverse xs

instance Traversable State where
  traverse f (S xs y zs) = liftA3 S (traverse f xs) (f y) (traverse f zs)

instance Loadable State where
  load = mapM
  unload = mapM_

instance Loadable Mapping where
  load f (M k a) = M k <$> f a
  unload f (M k a) = f a

lesson04 :: IO ()
lesson04
    = (^.^) sdlInitVideo ()                     -- initialize SDL
    $ \() -> (^.^) defaultWindow "Lesson04"     -- create window
    $ \w -> (^.^) surface w                     -- get surface from given window
    $ \s -> (^.^/) bmpSurface mapPicPath
    $ \l@(S xs y@(M k sur) zs) -> update_ sur $ eventHandle_ checkDefaultQuit
      (\f t -> do
               let newS = eventToSurface l f
               return $ fromMaybe t newS
      )
      (\f t -> do
               SDL.surfaceBlit t Nothing s Nothing
               SDL.updateWindowSurface w
      )

-- to decide a surface to blit from given event info.
eventToSurface :: State (MapPic)
               -> SDL.Event
               -> Maybe SDL.Surface
eventToSurface s@(S xs (M i sur) zs) event =
    case input of
      Just LT -> eventToSurface (right s) event
      Just GT -> eventToSurface (left s) event
      Just EQ -> Just sur
      Nothing -> Nothing
      where input = compare i <$> (getKeycode event >>= toInput mapping)
