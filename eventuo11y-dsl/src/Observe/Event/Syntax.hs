{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Description : Syntax helpers for eventuo11y interfaces
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- Syntax helpers for eventuo11y interfaces.
module Observe.Event.Syntax where

infixr 4 ≔

-- | A type class for common syntax for types that are key-value-like.
--
-- For example, the appropriate 'RecordField' instances allow for
-- @[ "bytes", "asked" ] ≔ ''ByteCount@ and @[ "bytes", "actual" ] ≔ [t|Maybe ByteCount|]@
-- to both construct 'Observe.Event.DSL.FieldConstructorSpec's, the former creating a constructor
-- @BytesAsked@ taking a 'System.Posix.Types.ByteCount' and the latter a constructor
-- @BytesActual@ taking a 'Maybe' 'System.Posix.Types.ByteCount'.
class RecordField k v a where
  (≔) :: k -> v -> a
