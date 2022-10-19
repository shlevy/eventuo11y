{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Description : Compile the "Observe.Event.DSL" and generate "Observe.Event.Render.JSON" instances
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
module Observe.Event.Render.JSON.DSL.Compile (compile) where

import Data.Aeson
import GHC.Exts
import Language.Haskell.TH
import Language.Haskell.TH.Syntax.Compat as THC
import Observe.Event.DSL
import qualified Observe.Event.DSL.Compile as DSL
import Observe.Event.Render.JSON

conPCompat :: Name -> [Pat] -> Pat
#if MIN_VERSION_template_haskell(2,18,0)
conPCompat n ps = ConP n [] ps
#else
conPCompat = ConP
#endif

-- | Compile a 'SelectorSpec' to type definitions, with a 'DefaultRenderSelectorJSON' instance.
--
-- Assumes leaf types (i.e., those in 'SimpleType' or a 'FieldConstructorSpec') are 'ToJSON', and
-- that 'Inject'ed selectors have a 'DefaultRenderSelectorJSON' instance.
compile :: (THC.Quote m) => SelectorSpec -> m [Dec]
compile s@(SelectorSpec selectorNameBase selectors) = do
  -- Walks the selectors twice, will fix when SelectorSpec is extensible (e.g. recursion-schemes)
  baseDecs <- DSL.compile s
  let (renderSelectorClauses, decs) = foldr stepSelectors (mempty, baseDecs) selectors
      selectorInstance =
        InstanceD
          Nothing
          []
          (AppT (ConT ''DefaultRenderSelectorJSON) (ConT selectorName))
          [FunD 'defaultRenderSelectorJSON renderSelectorClauses]
  pure $ selectorInstance : decs
  where
    -- Deduplicate this with e11y-dsl when extending language
    selectorName = mkName $ upperCamel selectorNameBase <> "Selector"

    stepSelectors (SelectorConstructorSpec nm NoFields) (renderSelectorClauses, decs) = (c : renderSelectorClauses, decs)
      where
        c =
          Clause
            [conPCompat (mkName $ upperCamel nm) []]
            ( NormalB
                ( TupE
                    [ Just . LitE $ StringL (kebab nm),
                      Just $ VarE 'defaultRenderFieldJSON
                    ]
                )
            )
            []
    stepSelectors (SelectorConstructorSpec nm (Inject _)) (renderSelectorClauses, decs) = (c : renderSelectorClauses, decs)
      where
        keyNm = mkName "key"
        renderNm = mkName "render"
        selNm = mkName "sel"
        c =
          Clause
            [conPCompat (mkName $ upperCamel nm) [VarP selNm]]
            ( NormalB
                ( TupE
                    [ Just $ InfixE (Just . LitE $ StringL (kebab nm <> ":")) (VarE '(<>)) (Just $ VarE keyNm),
                      Just $ VarE renderNm
                    ]
                )
            )
            [ ValD (TupP [VarP keyNm, VarP renderNm]) (NormalB $ AppE (VarE 'defaultRenderSelectorJSON) (VarE selNm)) []
            ]
    -- TODO handle case where field is not ToJSON
    stepSelectors (SelectorConstructorSpec nm (SimpleType _)) (renderSelectorClauses, decs) = (c : renderSelectorClauses, decs)
      where
        xNm = mkName "x"
        c =
          Clause
            [conPCompat (mkName $ upperCamel nm) []]
            ( NormalB
                ( TupE
                    [ Just . LitE $ StringL (kebab nm),
                      Just
                        . LamE
                          [ VarP xNm
                          ]
                        $ TupE
                          [ Just . LitE $ StringL "val", -- Extend SimpleType with field name
                            Just $ AppE (VarE 'toJSON) (VarE xNm)
                          ]
                    ]
                )
            )
            []
    stepSelectors (SelectorConstructorSpec nm (Specified fieldSpec)) (renderSelectorClauses, decs) = (c : renderSelectorClauses, fieldDec : decs)
      where
        c =
          Clause
            [conPCompat (mkName $ upperCamel nm) []]
            ( NormalB
                ( TupE
                    [ Just . LitE $ StringL (kebab nm),
                      Just $ VarE 'defaultRenderFieldJSON
                    ]
                )
            )
            []
        fieldDec = compileFieldSpec fieldSpec

compileFieldSpec :: FieldSpec -> Dec
compileFieldSpec (FieldSpec fieldNameBase fields) =
  InstanceD
    Nothing
    []
    (AppT (ConT ''DefaultRenderFieldJSON) (ConT fieldName))
    [FunD 'defaultRenderFieldJSON (renderFieldClause <$> fields)]
  where
    -- Deduplicate this with e11y-dsl when extending language
    fieldName = mkName $ upperCamel fieldNameBase <> "Field"
    renderFieldClause (FieldConstructorSpec ctorNm ts) =
      Clause
        [conPCompat (mkName $ upperCamel ctorNm) (map VarP argNms)]
        ( NormalB
            ( TupE
                [ Just . LitE $ StringL (kebab ctorNm),
                  Just $ valE
                ]
            )
        )
        []
      where
        len = length ts
        argNms = fmap (\idx -> mkName $ "x" <> show idx) [1 .. len]
        valE = case argNms of
          [] -> ConE 'Null
          [argNm] -> AppE (VarE 'toJSON) (VarE argNm)
          _ ->
            AppE
              (ConE 'Object)
              ( AppE
                  (VarE 'fromList)
                  ( ListE . fst $
                      foldr
                        ( \argNm (jsonEs, idx) ->
                            ( TupE
                                [ Just . LitE $ StringL (show idx),
                                  Just $ AppE (VarE 'toJSON) (VarE argNm)
                                ] :
                              jsonEs,
                              idx - 1
                            )
                        )
                        ([], len)
                        argNms
                  )
              )
