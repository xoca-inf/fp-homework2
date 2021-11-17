module HW2.T1
  ( -- * Datatypes
    Annotated (..)
  , Except (..)
  , Fun (..)
  , List (..)
  , Option (..)
  , Pair (..)
  , Prioritised (..)
  , Quad (..)
  , Stream (..)
  , Tree (..)
    -- * map functions
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapQuad
  , mapStream
  , mapTree
  ) where

import Prelude ()
import Prelude (($))
import Prelude (undefined)

data Option a
  = None
  | Some a

data Pair a = P a a

data Quad a = Q a a a a

data Annotated e a = a :# e
infix 0 :#

data Except e a
  = Error e
  | Success a

data Prioritised a
  = Low a
  | Medium a
  | High a

data Stream a = a :> Stream a
infixr 5 :>

data List a
  = Nil
  | a :. List a
infixr 5 :.

newtype Fun i a = F (i -> a)

data Tree a
  = Leaf
  | Branch
    (Tree a)
    a
    (Tree a)

mapOption :: (a -> b) -> (Option a -> Option b)
mapOption f (Some a) = Some $ f a
mapOption f None     = None

mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair f (P x y) = P (f x) (f y)

mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad f (Q a1 a2 a3 a4) = Q (f a1) (f a2) (f a3) (f a4)

mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated f (a :# e) = f a :# e

mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept f (Error e)   = Error e
mapExcept f (Success a) = Success $ f a

mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised f (Low a)    = Low $ f a
mapPrioritised f (Medium a) = Medium $ f a
mapPrioritised f (High a)   = High $ f a

mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream f (x :> xs) = f x :> mapStream f xs

mapList :: (a -> b) -> (List a -> List b)
mapList f Nil = Nil
mapList f (x :. xs) = f x :. mapList f xs

mapFun :: (a -> b) -> (Fun i a -> Fun i b)
mapFun f (F fun) = F $ \i -> f (fun i)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree f Leaf = Leaf
mapTree f (Branch left x right) = 
  Branch (mapTree f left) (f x) (mapTree f right) 