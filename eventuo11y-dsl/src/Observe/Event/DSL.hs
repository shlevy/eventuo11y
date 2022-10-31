{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-- Duplication due to compatibility for AnyQuote
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

-- |
-- Description : DSL for generating 'Observe.Event.Event' fields and selectors
-- Copyright   : Copyright 2022 Shea Levy.
-- License     : Apache-2.0
-- Maintainer  : shea@shealevy.com
--
-- DSL for generating 'Observe.Event.Event' fields and selectors.
--
-- Typical entrypoint is 'SelectorSpec'.
--
-- See [Example.hs](https://github.com/shlevy/eventuo11y/tree/v0.5.0.0/Example.hs) for an idiomatic example.
--
-- See "Observe.Event.DSL.Compile" to compile this into the relevant types.
module Observe.Event.DSL
  ( -- * The core AST
    SelectorSpec (..),
    SelectorConstructorSpec (..),
    SelectorField (..),
    FieldSpec (..),
    FieldConstructorSpec (..),

    -- * Syntax
    RecordField (..),

    -- * Miscellaneous helpers

    -- ** Quote polymorphism
    AnyQuote (..),
    toQuote,
    AnyType,

    -- ** Names
    ExplodedName,
    upperCamel,
    lowerCamel,
    kebab,
    NonEmptyString ((:|:)),
    nonEmptyToString,
  )
where

#if MIN_VERSION_template_haskell(2,18,0)
import Control.Applicative
#endif
import Data.Char
import Data.List
import Data.List.NonEmpty hiding (fromList, toList)
import Data.String
import GHC.Exts
import Language.Haskell.TH
import Observe.Event.Syntax

-- | A specification for an 'Observe.Event.Event' selector type
data SelectorSpec
  = SelectorSpec
      !ExplodedName
      -- ^ The base name of the generated type. @Selector@ will be appended.
      ![SelectorConstructorSpec]
      -- ^ Constructors for the selector type.

-- | A specification for a single constructor for a selector
--
-- End users probably want to use 'RecordField' to create
-- 'SelectorConstructorSpec's.
data SelectorConstructorSpec
  = SelectorConstructorSpec
      !ExplodedName
      -- ^ The name of the constructor
      !SelectorField
      -- ^ The type of fields associated with this selector

-- | Ways to specify the field for a selector.
data SelectorField
  = -- | The field is itself specified with the DSL.
    --
    -- The field type will be generated alongside the selector type.
    --
    -- End users probably want to use the 'RecordField' 'ExplodedName'
    -- 'FieldSpec' 'SelectorConstructorSpec' instance for 'Specified'
    -- 'SelectorField's.
    Specified !FieldSpec
  | -- | The field type is simply a preexisting type, typically not eventuo11y-aware.
    --
    -- End users probably want to use the 'RecordField' 'ExplodedName'
    -- 'Name' 'SelectorConstructorSpec' or 'RecordField' 'ExplodedName'
    -- 'AnyType' 'SelectorConstructorSpec' instances for 'SimpleType'
    -- 'SelectorField's
    SimpleType !AnyType
  | -- | This selector is a natural injection from a different selector type.
    --
    -- This is typically used to call library code with its own selector types.
    Inject !Name
  | -- | Events selected by this selector have no fields.
    --
    -- This may be useful purely to add timing to some event, or
    -- to create an event that is parent and/or proximate to other
    -- events.
    NoFields

-- | A specification for an 'Observe.Event.Event' field type.
data FieldSpec
  = FieldSpec
      !ExplodedName
      -- ^ The base name of the field. @Field@ will be appended.
      ![FieldConstructorSpec]
      -- ^ Constructors of this field type.

-- | A specification for a single constructor for a field
--
-- End users probably want to use 'RecordField' to create
-- 'FieldConstructorSpec's.
data FieldConstructorSpec
  = FieldConstructorSpec
      !ExplodedName
      -- ^ The name of the constructor
      !(NonEmpty AnyType)
      -- ^ The types of the arguments to the constructor.

-- | e.g. @"foo" ≔ NoFields@
instance (a ~ ExplodedName) => RecordField a SelectorField SelectorConstructorSpec where
  (≔) = SelectorConstructorSpec

-- | e.g. @"foo" ≔ FieldSpec ...@
instance (a ~ ExplodedName) => RecordField a FieldSpec SelectorConstructorSpec where
  k ≔ v = k ≔ Specified v

-- | e.g. @"foo" ≔ [t|Maybe Int|]@
instance (a ~ ExplodedName, m ~ AnyQuote) => RecordField a (m Type) SelectorConstructorSpec where
  k ≔ v = k ≔ SimpleType v

-- | e.g. @"foo" ≔ ''Int@
instance (a ~ ExplodedName) => RecordField a Name SelectorConstructorSpec where
  k ≔ v = k ≔ (pure $ ConT v)

-- | e.g. @"foo" ≔ [t|Int] :| [ [t|Bool], [t|Char] ]@
instance (a ~ ExplodedName, m ~ AnyQuote) => RecordField a (NonEmpty (m Type)) FieldConstructorSpec where
  (≔) = FieldConstructorSpec

-- | e.g. @"foo" ≔ [t|Maybe Int]@
instance (a ~ ExplodedName, m ~ AnyQuote) => RecordField a (m Type) FieldConstructorSpec where
  k ≔ v = k ≔ (v :| [])

-- | e.g. @"foo" ≔ [''Int, ''Char]@
instance (a ~ ExplodedName) => RecordField a [Name] FieldConstructorSpec where
  k ≔ v = k ≔ (pure . ConT <$> fromList @(NonEmpty _) v)

-- | e.g. @"foo" ≔ ''Int@
instance (a ~ ExplodedName) => RecordField a Name FieldConstructorSpec where
  k ≔ v = k ≔ ((pure $ ConT v) :| [])

#if MIN_VERSION_template_haskell(2,18,0)
-- | A concrete type for TH quotes that retains full 'Quote' polymorphism
--
-- Prior to @template-haskell@ @2.18@, this is just an alias for 'Q'
newtype AnyQuote a = AnyQuote
  { -- | Extract this value in a particular 'Quote' monad
    --
    -- Prior to @template-haskell@ @2.18@, this projects into 'Q'.
    toQuote :: forall m. Quote m => m a
  } deriving (Functor)

instance Applicative AnyQuote where
  pure x = AnyQuote $ pure x
  (AnyQuote f) <*> (AnyQuote x) = AnyQuote $ f <*> x
  liftA2 f (AnyQuote x) (AnyQuote y) = AnyQuote $ liftA2 f x y
  (AnyQuote x) *> (AnyQuote y) = AnyQuote $ x *> y
  (AnyQuote x) <* (AnyQuote y) = AnyQuote $ x <* y

instance Monad AnyQuote where
  (AnyQuote x) >>= f = AnyQuote $ do
    x' <- x
    let AnyQuote res = f x'
    res

instance Quote AnyQuote where
  newName s = AnyQuote $ newName s

#else
-- | A type alias for TH quotes
type AnyQuote = Q

toQuote :: AnyQuote a -> Q a
toQuote = id
#endif

-- | A 'Type' in any 'Quote'
type AnyType = AnyQuote Type

-- | A name for some element, broken up into words.
--
-- Different elements will use this differently. For example, using
-- @[ "foo", "bar" ]@ in a 'SelectorSpec' would result in a type named
-- @FooBarSelector@, while using it in a 'FieldSpec' might cause a
-- renderer generator to give the field the key @foo-bar@.
newtype ExplodedName = ExplodedName (NonEmpty NonEmptyString)

-- | Must be non-empty.
instance IsList ExplodedName where
  type Item ExplodedName = NonEmptyString
  fromList = coerce . fromList @(NonEmpty _)
  toList = toList @(NonEmpty _) . coerce

-- | A singleton 'ExplodedName'.
instance IsString ExplodedName where
  fromString = fromList . (: []) . fromString

-- | Convert an 'ExplodedName' to UpperCamelCase.
upperCamel :: (IsList a, Item a ~ NonEmptyString) => a -> String
upperCamel = concat . fmap (\(hd :|: tl) -> toUpper hd : tl) . toList

-- | Convert an 'ExplodedName' to lowerCamelCase
lowerCamel :: ExplodedName -> String
lowerCamel (ExplodedName ((hd :|: tl) :| rest)) =
  (toLower hd : tl)
    <> upperCamel rest

-- | Convert an 'ExplodedName' to kebab-case
kebab :: ExplodedName -> String
kebab = intercalate "-" . fmap (\(hd :|: tl) -> toLower hd : tl) . toList

-- | Self-explanatory
newtype NonEmptyString = NonEmptyString (NonEmpty Char)

-- | Must be non-empty
instance IsList NonEmptyString where
  type Item NonEmptyString = Char
  fromList = coerce . fromList @(NonEmpty _)
  toList = toList @(NonEmpty _) . coerce

{-# COMPLETE (:|:) #-}

pattern (:|:) :: Char -> String -> NonEmptyString
pattern (:|:) hd tl <- NonEmptyString (hd :| tl)

-- | Must be non-empty
instance IsString NonEmptyString where
  fromString = fromList

-- | Self-explanatory
nonEmptyToString :: NonEmptyString -> String
nonEmptyToString = toList
