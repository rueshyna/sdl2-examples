{- Keyboard events handling -}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Lesson04 where
--
import qualified SDL
--
import Data.Maybe
--
import PixelPen
import Data.ListZipper
--

data Input = IDefault
           | IDown
           | IUp
           | IRight
           | ILeft
           deriving (Show, Eq, Ord, Enum)

type Mapping a = (Input, a)
type MapKeycode = Mapping SDL.Keycode
type MapSurface = Mapping SDL.Surface
newtype ZipperMap a = ZM (Zipper (Mapping a))
               deriving (Functor, Foldable, Traversable)

mapPicPath :: ZipperMap String
mapPicPath = ZM
           $ Z [] (IDefault, "./img/press.bmp")
                [ (IDown,    "./img/down.bmp")
                , (IUp,      "./img/up.bmp")
                , (IRight,   "./img/right.bmp")
                , (ILeft,    "./img/left.bmp")
                ]

mapping :: [ MapKeycode ]
mapping = [ (IDefault, SDL.KeycodeEscape)
          , (IDown,    SDL.KeycodeDown)
          , (IUp,      SDL.KeycodeUp)
          , (IRight,   SDL.KeycodeRight)
          , (ILeft,    SDL.KeycodeLeft)
          ]

toInput :: [ MapKeycode ] -> SDL.Keycode -> Maybe Input
toInput [] _ = Nothing
toInput ((i, k):xs) mk
    | k == mk = Just i
    | k /= mk = toInput xs mk

-- to decide a surface to blit from given event info.
eventToSurface :: Zipper MapSurface
               -> SDL.Event
               -> Maybe SDL.Surface
eventToSurface s@(Z xs (i,sur) zs) event =
    case input of
      Just LT -> eventToSurface (right s) event
      Just GT -> eventToSurface (left s) event
      Just EQ -> Just sur
      Nothing -> Nothing
      where input = compare i <$> (getKeycode event >>= toInput mapping)

lesson04 :: IO ()
lesson04
    = (^.^) sdlInitVideo ()                     -- initialize SDL
    $ \() -> (^.^) defaultWindow "Lesson04"     -- create window
    $ \w -> (^.^) surface w                     -- get surface from given window
    $ \s -> (^.^/) bmpSurface mapPicPath
    $ \(ZM l@(Z xs (k, sur) zs)) -> update_ sur $ eventHandle_ checkDefaultQuit
      (\f t -> do
               let newS = eventToSurface l f
               return $ fromMaybe t newS
      )
      (\f t -> do
               SDL.surfaceBlit t Nothing s Nothing
               SDL.updateWindowSurface w
      )
