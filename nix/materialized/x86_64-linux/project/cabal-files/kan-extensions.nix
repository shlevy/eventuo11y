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
      identifier = { name = "kan-extensions"; version = "5.2.5"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2016 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/kan-extensions/";
      url = "";
      synopsis = "Kan extensions, Kan lifts, the Yoneda lemma, and (co)density (co)monads";
      description = "Kan extensions, Kan lifts, various forms of the Yoneda lemma, and (co)density (co)monads.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."adjunctions" or (errorHandler.buildDepError "adjunctions"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."invariant" or (errorHandler.buildDepError "invariant"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."fail" or (errorHandler.buildDepError "fail"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/kan-extensions-5.2.5.tar.gz";
      sha256 = "b914dccc040caf1d8764b99df1028dad3e4fdf46c262192e54b59c9da66ead22";
      });
    }) // {
    package-description-override = "name:          kan-extensions\r\ncategory:      Data Structures, Monads, Comonads, Functors\r\nversion:       5.2.5\r\nx-revision: 1\r\nlicense:       BSD3\r\ncabal-version: >= 1.10\r\nlicense-file:  LICENSE\r\nauthor:        Edward A. Kmett\r\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\r\nstability:     provisional\r\nhomepage:      http://github.com/ekmett/kan-extensions/\r\nbug-reports:   http://github.com/ekmett/kan-extensions/issues\r\ncopyright:     Copyright (C) 2008-2016 Edward A. Kmett\r\nsynopsis:      Kan extensions, Kan lifts, the Yoneda lemma, and (co)density (co)monads\r\ndescription:   Kan extensions, Kan lifts, various forms of the Yoneda lemma, and (co)density (co)monads.\r\nbuild-type:    Simple\r\ntested-with:   GHC == 7.4.2\r\n             , GHC == 7.6.3\r\n             , GHC == 7.8.4\r\n             , GHC == 7.10.3\r\n             , GHC == 8.0.2\r\n             , GHC == 8.2.2\r\n             , GHC == 8.4.4\r\n             , GHC == 8.6.5\r\n             , GHC == 8.8.4\r\n             , GHC == 8.10.7\r\n             , GHC == 9.0.2\r\n             , GHC == 9.2.2\r\n\r\nextra-source-files:\r\n  .gitignore\r\n  .ghci\r\n  .vim.custom\r\n  CHANGELOG.markdown\r\n  README.markdown\r\n  include/kan-extensions-common.h\r\n\r\nsource-repository head\r\n  type: git\r\n  location: git://github.com/ekmett/kan-extensions.git\r\n\r\nlibrary\r\n  hs-source-dirs: src\r\n  include-dirs: include\r\n  includes: kan-extensions-common.h\r\n\r\n  other-extensions:\r\n    CPP\r\n    MultiParamTypeClasses\r\n    GADTs\r\n    Rank2Types,\r\n    FlexibleInstances\r\n    FlexibleContexts\r\n    UndecidableInstances\r\n    TypeFamilies\r\n\r\n  build-depends:\r\n    adjunctions         >= 4.2     && < 5,\r\n    array               >= 0.3.0.2 && < 0.6,\r\n    base                >= 4.4     && < 5,\r\n    comonad             >= 4       && < 6,\r\n    containers          >= 0.4     && < 0.7,\r\n    contravariant       >= 1       && < 2,\r\n    distributive        >= 0.2.2   && < 1,\r\n    invariant           >= 0.1     && < 1,\r\n    free                >= 4       && < 6,\r\n    mtl                 >= 2.0.1   && < 2.4,\r\n    profunctors         >= 5       && < 6,\r\n    semigroupoids       >= 4       && < 7,\r\n    tagged              >= 0.7.2   && < 1,\r\n    transformers        >= 0.2     && < 0.7,\r\n    transformers-compat >= 0.3     && < 0.8\r\n\r\n  exposed-modules:\r\n    Control.Comonad.Density\r\n    Control.Monad.Co\r\n    Control.Monad.Codensity\r\n    Data.Functor.Contravariant.Day\r\n    Data.Functor.Contravariant.Yoneda\r\n    Data.Functor.Contravariant.Coyoneda\r\n    Data.Functor.Day\r\n    Data.Functor.Day.Curried\r\n    Data.Functor.Invariant.Day\r\n    Data.Functor.Kan.Lan\r\n    Data.Functor.Kan.Ran\r\n    Data.Functor.Yoneda\r\n    Data.Functor.Coyoneda\r\n\r\n  ghc-options: -Wall\r\n  default-language: Haskell2010\r\n\r\n  if impl(ghc >= 7.10)\r\n    ghc-options: -fno-warn-trustworthy-safe\r\n\r\n  -- See https://ghc.haskell.org/trac/ghc/wiki/Migration/8.0#base-4.9.0.0\r\n  if impl(ghc >= 8.0)\r\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\r\n\r\n    if !impl(ghc >= 8.8)\r\n      ghc-options: -Wnoncanonical-monadfail-instances\r\n  else\r\n    build-depends: fail >= 4.9 && < 5\r\n";
    }