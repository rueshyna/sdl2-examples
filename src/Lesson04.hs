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

data Mapping = M Input SDL.Keycode
data Link a = L Input a
data State a = S [a] a [a]

preLink' :: State (Link String)
preLink' = S [] ( L IDefault "./img/press.bmp")
                [ L IDown    "./img/down.bmp"
                , L IUp      "./img/up.bmp"
                , L IRight   "./img/right.bmp"
                , L ILeft    "./img/left.bmp"
                ]

mapping :: [ Mapping ]
mapping = [ M IDefault SDL.KeycodeEscape
          , M IDown    SDL.KeycodeDown
          , M IUp      SDL.KeycodeUp
          , M IRight   SDL.KeycodeRight
          , M ILeft    SDL.KeycodeLeft
          ]

toInput :: [ Mapping ] -> SDL.Keycode -> Maybe Input
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

instance Loadable Link where
  load f (L k a) = L k <$> f a
  unload f (L k a) = f a

lesson04 :: IO ()
lesson04
    = (^.^) sdlInitVideo ()                     -- initialize SDL
    $ \() -> (^.^) defaultWindow "Lesson04"     -- create window
    $ \w -> (^.^) surface w                     -- get surface from given window
    $ \s -> (^.^/) bmpSurface preLink'
    $ \l@(S xs y@(L k sur) zs) -> update_ sur $ eventHandle_ checkDefaultQuit
      (\f t -> do
               let newS = eventToSurface l f
               return $ fromMaybe t newS
      )
      (\f t -> do
               SDL.surfaceBlit t Nothing s Nothing
               SDL.updateWindowSurface w
      )

-- to decide a surface to blit from given event info.
eventToSurface :: State (Link SDL.Surface)
               -> SDL.Event
               -> Maybe SDL.Surface
eventToSurface s@(S xs (L i sur) zs) event =
    case input of
      Just LT -> eventToSurface (right s) event
      Just GT -> eventToSurface (left s) event
      Just EQ -> Just sur
      Nothing -> Nothing
      where input = compare i <$> (getKeycode event >>= toInput mapping)
