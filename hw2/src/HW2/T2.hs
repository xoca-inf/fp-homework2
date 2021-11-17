module HW2.T2
  ( -- * dist functions
    distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
    -- * wrap functions
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some), Pair (P), Prioritised (High, Low, Medium), Quad (Q),
               Stream ((:>)))
import Prelude (Monoid, Semigroup, mempty, (<>), undefined, ($), head, tail, (++))

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption (Some _, None)   = None
distOption (None, Some _)   = None
distOption (None, None)     = None

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (x1 :# xs1, x2 :# xs2) = (x1, x2) :# (xs1 <> xs2)

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success a, Success b) = Success (a, b)
distExcept (Success _, Error e)   = Error e
distExcept (Error e1, Error _)    = Error (e1)
distExcept (Error e1, Success _)  = Error (e1)

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a,   High b  )   = High (a, b)
distPrioritised (High a,   Medium b)   = High (a, b)
distPrioritised (High a,   Low b   )   = High (a, b)
distPrioritised (Medium a, High b  )   = High (a, b)
distPrioritised (Medium a, Medium b)   = Medium (a, b)
distPrioritised (Medium a, Low b   )   = Medium (a, b)
distPrioritised (Low a,    High b  )   = High (a, b)
distPrioritised (Low a,    Medium b)   = Medium (a, b)
distPrioritised (Low a,    Low b   )   = Low (a, b)

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a1 :> a2, b1 :> b2) = (a1, b1) :> (distStream (a2, b2))

myDistList :: (a, List b, List (a, b)) -> List (a,b)
myDistList (a, c :. d, x) = myDistList(a, d, (a, c) :. x)
myDistList (_ , Nil, x) = x

myJoin :: (List(a, b), List (a, b)) -> List (a, b)
myJoin ((a :. b), c) = myJoin (b, (a :. c))
myJoin (Nil,      b) = b

distList :: (List a, List b) -> List (a, b)
distList (a1 :. a2, b1 :. b2) = myJoin ((myDistList(a1, b1 :. b2, Nil)), (distList (a2, b1 :. b2)))
distList (_, Nil)             = Nil
distList (Nil, _)             = Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F $ \i -> (f i, g i)

wrapOption :: a -> Option a
wrapOption a = Some a
wrapOption _ = None

wrapPair :: a -> Pair a
wrapPair a = P a a

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept :: a -> Except e a
wrapExcept a = Success a

wrapPrioritised :: a -> Prioritised a
wrapPrioritised a = Low a

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

wrapList :: a -> List a
wrapList a   = a :. Nil

wrapFun :: a -> Fun i a
wrapFun a = F $ \i -> a