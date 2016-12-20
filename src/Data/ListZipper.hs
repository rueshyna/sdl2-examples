module Data.ListZipper where

import Control.Applicative(liftA3)

data Zipper a = Z [a] a [a]


left :: Zipper a -> Zipper a
left  (Z [] c rs) = Z [] c rs
left  (Z (l:ls) c rs) = Z ls l (c:rs)

right :: Zipper a -> Zipper a
right (Z ls c []) = Z ls c []
right (Z ls c (r:rs)) = Z (c:ls) r rs

instance Functor Zipper where
  fmap f (Z xs y zs) = Z (map f xs) (f y) (map f zs)

instance Foldable Zipper where
  foldr f w (Z xs y zs) = foldr f (f y (foldr f w zs)) $ reverse xs

instance Traversable Zipper where
  traverse f (Z xs y zs) = liftA3 Z (traverse f xs) (f y) (traverse f zs)
