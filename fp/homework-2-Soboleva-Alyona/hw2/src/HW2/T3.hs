module HW2.T3(
    joinOption,
    joinExcept,
    joinAnnotated,
    joinList,
    joinFun
) where

import HW2.T1

joinOption    :: Option (Option a) -> Option a
joinOption (Some None) = None
joinOption (Some (Some a)) = Some a 
joinOption None = None

joinExcept    :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e
joinExcept (Success (Error e)) = Error e
joinExcept (Success (Success a)) = Success a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# (e1 <> e2) 

joinListHelp :: List a -> List a -> List a 
joinListHelp Nil a = a 
joinListHelp _ Nil = Nil
joinListHelp (a :. la) b = a :. (joinListHelp la b)

joinList      :: List (List a) -> List a
joinList Nil = Nil
joinList (a :. Nil) = a
joinList (Nil :. t) = joinList t
joinList (a :. b) = joinListHelp a (joinList b)


joinFun       :: Fun i (Fun i a) -> Fun i a
joinFun (F innerF)  = F $ \i ->
    let F innerF' = innerF i 
    in innerF' i 
