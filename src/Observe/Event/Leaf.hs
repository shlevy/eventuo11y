{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Observe.Event.Leaf where

import Data.Kind

import Observe.Event.Core

type NoSubEvents :: EventSpec -> Type
newtype NoSubEvents es = MkNoSubEvents (forall a . a)

absurdNSE :: NoSubEvents es -> a
absurdNSE (MkNoSubEvents a) = a

type LeafEventSpec :: Type -> EventSpec
type LeafEventSpec field = 'MkEventSpec field NoSubEvents

type LeafEvent :: (Type -> Type) -> Type -> Type -> Type
type LeafEvent m r field = Event m r (LeafEventSpec field)

type NoFields :: Type
newtype NoFields = MkNoFields (forall a . a)

absurdNF :: NoFields -> a
absurdNF (MkNoFields a) = a

type EmptyEventSpec :: EventSpec
type EmptyEventSpec = LeafEventSpec NoFields

type EmptyEvent :: (Type -> Type) -> Type -> Type
type EmptyEvent m r = Event m r EmptyEventSpec
