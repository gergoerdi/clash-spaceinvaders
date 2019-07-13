{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeApplications #-}
{-# LANGUAGE StandaloneDeriving, DeriveFunctor #-}
module Hardware.Intel8080.Star
       ( Step(..)
       , Star(End), (>:>), (>++>)
       , stepsOf
       ) where

import Prelude ()
import Clash.Prelude
import Data.Singletons
import Data.Proxy

data Ends k
    = Empty
    | NonEmpty k k

class (SingI (MeetOf a b)) => Meet (a :: Maybe k) (b :: Maybe k) where
    type MeetOf a b :: Maybe k

instance Meet Nothing Nothing where
    type MeetOf Nothing Nothing = Nothing

instance (SingI a) => Meet (Just a) Nothing where
    type MeetOf (Just a) Nothing = Just a

instance (SingI b) => Meet Nothing (Just b) where
    type MeetOf Nothing (Just b) = Just b

instance (SingI a) => Meet (Just a) (Just a) where
    type MeetOf (Just a) (Just a) = Just a

class Append (ends :: Ends (Maybe k)) (ends' :: Ends (Maybe k)) where
    type AppendOf ends ends' :: Ends (Maybe k)

instance Append Empty ends' where
    type AppendOf Empty ends' = ends'

instance Append ends Empty where
    type AppendOf ends Empty = ends

instance (Meet mid mid') => Append (NonEmpty first mid) (NonEmpty mid' last) where
    type AppendOf (NonEmpty first mid) (NonEmpty mid' last) = NonEmpty first last

data Step (before :: k) (after :: k) a where
    Step :: (SingI before, SingI after) => a -> Step before after a
deriving instance Functor (Step before after)

data Star (ends :: Ends k) (n :: Nat) a where
    End :: Star Empty 0 a
    Plus :: (SingKind k, SingI before, SingI after) => Vec n (a, Demote k) -> a -> Star (NonEmpty before after :: Ends k) (n + 1) a
deriving instance Functor (Star end n)

after :: forall k (before :: Maybe k) after mid end n a. (SingKind k, Meet after mid) => Step before after a -> Star (NonEmpty mid end) n a -> Maybe (Demote k)
after _ _ = demote @(MeetOf after mid)

infixr 5 >:>
(>:>)
    :: forall (before :: Maybe k) (after :: Maybe k) (ends :: Ends (Maybe k)) n a. ()
    => (SingKind k, Append (NonEmpty before after) ends, Meet after (StartOf ends))
    -- => (SingI (MeetOf after (StartOf ends)))
    => Step before after a
    -> Star ends n a
    -> Star (AppendOf (NonEmpty before after) ends) (n + 1) a
(Step x) >:> End = Plus Nil x
s@(Step x) >:> ys0@(Plus ys yn) = Plus ((x, after s ys0) :> ys) yn

type family EndOf (ends :: Ends k) where
    EndOf (NonEmpty a b) = b

type family StartOf (ends :: Ends (Maybe k)) where
    StartOf Empty = Nothing
    StartOf (NonEmpty a b) = a

mid :: forall k (start :: Maybe k) mid mid' end n m a. (SingKind k, Meet mid mid') => Star (NonEmpty start mid) n a -> Star (NonEmpty mid' end) m a -> Maybe (Demote k)
mid _ _ = demote @(MeetOf mid mid')

infixr 5 >++>
(>++>) :: forall (ends :: Ends (Maybe k)) ends' n m a. (SingKind k, Show (Demote k), Append ends ends', Meet (EndOf ends) (StartOf ends')) => Star ends n a -> Star ends' m a -> Star (AppendOf ends ends') (n + m) a
End >++> ys = ys
xs >++> End = xs
xs0@(Plus xs xn) >++> ys0@(Plus ys yn) = Plus (xs ++ singleton (xn, mid xs0 ys0) ++ ys) yn

stepsOf :: forall (ends :: Ends (Maybe k)) n a. (SingKind k) => Star ends n a -> (Maybe (Demote k), Vec n (a, Maybe (Demote k)))
stepsOf End = (Nothing, Nil)
stepsOf (Plus xs xn) = (demote @(StartOf ends), xs ++ singleton (xn, demote @(EndOf ends)))
