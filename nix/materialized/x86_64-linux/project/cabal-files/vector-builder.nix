{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "vector-builder"; version = "0.3.8.4"; };
      license = "MIT";
      copyright = "(c) 2016, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/vector-builder";
      url = "";
      synopsis = "Vector builder";
      description = "An API for efficient and convenient construction of vectors.\nIt provides the composable `Builder` abstraction, which has instances of the `Monoid` and `Semigroup` classes.\n\n[Usage]\n\nFirst you use the `Builder` abstraction to specify the structure of the vector.\nThen you execute the builder to actually produce the vector.\n\n[Example]\n\nThe following code shows how you can efficiently concatenate different datastructures into a single immutable vector:\n\n>\n>import qualified Data.Vector as A\n>import qualified VectorBuilder.Builder as B\n>import qualified VectorBuilder.Vector as C\n>\n>\n>myVector :: A.Vector a -> [a] -> a -> A.Vector a\n>myVector vector list element =\n>  C.build builder\n>  where\n>    builder =\n>      B.vector vector <>\n>      B.foldable list <>\n>      B.singleton element";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ];
        buildable = true;
        };
      tests = {
        "tests" = {
          depends = [
            (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."rerebase" or (errorHandler.buildDepError "rerebase"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."vector-builder" or (errorHandler.buildDepError "vector-builder"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vector-builder-0.3.8.4.tar.gz";
      sha256 = "bccbba2f6bba91235bd13fabfc81383ee8e57ba1c7430d4a8307281664b1823d";
      });
    }) // {
    package-description-override = "name:\n  vector-builder\nversion:\n  0.3.8.4\nsynopsis:\n  Vector builder\ndescription:\n  An API for efficient and convenient construction of vectors.\n  It provides the composable `Builder` abstraction, which has instances of the `Monoid` and `Semigroup` classes.\n  .\n  [Usage]\n  .\n  First you use the `Builder` abstraction to specify the structure of the vector.\n  Then you execute the builder to actually produce the vector.\n  .\n  [Example]\n  .\n  The following code shows how you can efficiently concatenate different datastructures into a single immutable vector:\n  .\n  >\n  >import qualified Data.Vector as A\n  >import qualified VectorBuilder.Builder as B\n  >import qualified VectorBuilder.Vector as C\n  >\n  >\n  >myVector :: A.Vector a -> [a] -> a -> A.Vector a\n  >myVector vector list element =\n  >  C.build builder\n  >  where\n  >    builder =\n  >      B.vector vector <>\n  >      B.foldable list <>\n  >      B.singleton element\ncategory:\n  Vector\nhomepage:\n  https://github.com/nikita-volkov/vector-builder\nbug-reports:\n  https://github.com/nikita-volkov/vector-builder/issues\nauthor:\n  Nikita Volkov <nikita.y.volkov@mail.ru>\nmaintainer:\n  Nikita Volkov <nikita.y.volkov@mail.ru>\ncopyright:\n  (c) 2016, Nikita Volkov\nlicense:\n  MIT\nlicense-file:\n  LICENSE\nbuild-type:\n  Simple\ncabal-version:\n  >=1.10\n\nsource-repository head\n  type:\n    git\n  location:\n    git://github.com/nikita-volkov/vector-builder.git\n\nlibrary\n  hs-source-dirs:\n    library\n  default-extensions:\n    Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language:\n    Haskell2010\n  exposed-modules:\n    VectorBuilder.Builder\n    VectorBuilder.MVector\n    VectorBuilder.Vector\n    VectorBuilder.MonadPlus\n    VectorBuilder.Alternative\n  other-modules:\n    VectorBuilder.Prelude\n    VectorBuilder.Core.Update\n    VectorBuilder.Core.Builder\n  build-depends:\n    vector >=0.12 && <0.14,\n    base >=4.10 && <5\n\ntest-suite tests\n  type:\n    exitcode-stdio-1.0\n  hs-source-dirs:\n    tests\n  main-is:\n    Main.hs\n  other-modules:\n    Main.Sample\n  default-extensions:\n    Arrows, BangPatterns, ConstraintKinds, DataKinds, DefaultSignatures, DeriveDataTypeable, DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable, EmptyDataDecls, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GADTs, GeneralizedNewtypeDeriving, LambdaCase, LiberalTypeSynonyms, MagicHash, MultiParamTypeClasses, MultiWayIf, NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings, PatternGuards, ParallelListComp, QuasiQuotes, RankNTypes, RecordWildCards, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TupleSections, TypeFamilies, TypeOperators, UnboxedTuples\n  default-language:\n    Haskell2010\n  build-depends:\n    attoparsec >=0.13 && <0.15,\n    QuickCheck >=2.8.1 && <3,\n    quickcheck-instances >=0.3.11 && <0.4,\n    rerebase <2,\n    tasty >=0.12 && <2,\n    tasty-hunit >=0.9 && <0.11,\n    tasty-quickcheck >=0.9 && <0.11,\n    vector-builder\n";
    }