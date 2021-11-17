module HW2.T3
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

import HW2.T1 (Annotated ((:#)), Except (Error, Success), Fun (F), List (Nil, (:.)),
               Option (None, Some))

joinOption :: (Option (Option a)) -> Option a
joinOption (Some (Some (a))) = (Some a)
joinOption (Some (None))     = None
joinOption (None)            = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success (Success a)) = Success a
joinExcept (Success (Error e))   = Error e
joinExcept (Error e)             = Error e

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = (a :# (e2 <> e1))
joinAnnotated ((a :# e1) :# mempty) = (a :# e1)

myJoin :: (List a) -> (List a) -> (List a)
myJoin Nil b = b
myJoin (x :. xs) b = let res = myJoin xs b in (x :. res)

joinList :: List (List a) -> List a
joinList  (Nil) = Nil
joinList (a :. b) = myJoin a  (joinList b)

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F func) = let getInner (F inner) i = inner i
  in F $ \i -> getInner (func i) i