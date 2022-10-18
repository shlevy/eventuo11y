{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Description : Compile the "Observe.Event.DSL" with TemplateHaskell
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
module Observe.Event.DSL.Compile (compile) where

import Control.Monad
import Data.Void
import GHC.Exts
import Language.Haskell.TH
import Language.Haskell.TH.Syntax.Compat as THC
import Observe.Event.DSL

-- | Compile a 'SelectorSpec' into appropriate declarations.
compile :: (THC.Quote m) => SelectorSpec -> m [Dec]
compile (SelectorSpec selectorNameBase selectors) = do
  (selectorCtors, defs) <- foldM stepSelectors mempty selectors
  let selectorDef =
        DataD [] selectorName [(plainTV $ mkName "f")] Nothing selectorCtors []
  pure $ selectorDef : defs
  where
    selectorName = mkName $ upperCamel selectorNameBase <> "Selector"

    stepSelectors (selectorCtors, defs) (SelectorConstructorSpec nm NoFields) = pure (ctor : selectorCtors, defs)
      where
        ctor = GadtC [mkName $ upperCamel nm] [] (AppT (ConT selectorName) (ConT ''Void))
    stepSelectors (selectorCtors, defs) (SelectorConstructorSpec nm (Inject t)) = pure (ctor : selectorCtors, defs)
      where
        varX = mkName "x"
        ctor =
          GadtC
            [mkName $ upperCamel nm]
            [(Bang NoSourceUnpackedness SourceStrict, AppT (ConT t) (VarT varX))]
            (AppT (ConT selectorName) (VarT varX))
    stepSelectors (selectorCtors, defs) (SelectorConstructorSpec nm (SimpleType (AnyQuote mt))) = do
      t <- mt
      let ctor = GadtC [mkName $ upperCamel nm] [] (AppT (ConT selectorName) t)
      pure (ctor : selectorCtors, defs)
    stepSelectors (selectorCtors, defs) (SelectorConstructorSpec nm (Specified fieldSpec)) = do
      (fieldName, fieldDef) <- compileFieldSpec fieldSpec
      let ctor = GadtC [mkName $ upperCamel nm] [] (AppT (ConT selectorName) (ConT fieldName))
      pure (ctor : selectorCtors, fieldDef : defs)

compileFieldSpec :: (THC.Quote m) => FieldSpec -> m (Name, Dec)
compileFieldSpec (FieldSpec fieldNameBase fields) = do
  ctors <- mapM fieldCtor fields
  pure
    ( fieldName,
      DataD [] fieldName [] Nothing ctors []
    )
  where
    makeBangType (AnyQuote mt) = do
      t <- mt
      pure (Bang NoSourceUnpackedness SourceStrict, t)

    fieldCtor (FieldConstructorSpec nm ts) = do
      let AnyQuote margs = toList <$> mapM makeBangType ts
      args <- margs
      pure $ NormalC (mkName $ upperCamel nm) args

    fieldName = mkName $ upperCamel fieldNameBase <> "Field"
