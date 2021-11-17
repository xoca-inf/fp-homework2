module HW2.T4
  ( -- * Datatypes
    Expr (..)
  , Prim (..)
  , State (..)
    -- * map functions
  , eval
  , joinState
  , mapState
  , modifyState
  , wrapState
  ) where

import qualified Control.Monad
import HW2.T1 (Annotated ((:#)), mapAnnotated)

data State s a = S
  { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f state = S $ mapAnnotated f . (runS state)

wrapState :: a -> State s a
wrapState a = S $ \s -> (a :# s)

joinState :: State s (State s a) -> State s a
--joinState (S state) = let getInner (ss -> a :# ss) = ss in S $ ()
joinState = undefined

modifyState :: (s -> s) -> State s ()
modifyState func = S $ \s -> () :# func s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- ^ (+)
  | Sub a a      -- ^ (-)
  | Mul a a      -- ^ (*)
  | Div a a      -- ^ (/)
  | Abs a        -- ^ abs
  | Sgn a        -- ^ signum

data Expr =
  Val Double
  | Op (Prim Expr)

instance Num Expr where
  x + y         = Op (Add x y)
  x * y         = Op (Mul x y)
  x - y         = Op (Sub x y)
  abs x         = Op (Abs x)
  signum x      = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
--  (x / y) + (a / b) = Op (Div (Op Add (Op (Mul x b) Op (Mul a y)) Op (Mul y b)))

eval :: Expr -> State [Prim Double] Double
eval = undefined