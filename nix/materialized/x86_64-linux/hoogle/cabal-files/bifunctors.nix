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
    flags = { tagged = true; };
    package = {
      specVersion = "1.24";
      identifier = { name = "bifunctors"; version = "5.6.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2008-2016 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/bifunctors/";
      url = "";
      synopsis = "Bifunctors";
      description = "Bifunctors.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."assoc" or (errorHandler.buildDepError "assoc"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ] ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.2")) [
          (hsPkgs."bifunctor-classes-compat" or (errorHandler.buildDepError "bifunctor-classes-compat"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ]) ++ (pkgs.lib).optional (flags.tagged) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "9.6")) (hsPkgs."foldable1-classes-compat" or (errorHandler.buildDepError "foldable1-classes-compat"));
        buildable = true;
        };
      tests = {
        "bifunctors-spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/bifunctors-5.6.1.tar.gz";
      sha256 = "06381471b5be16516a1b2c4b21a5101a3d991038bface8e0cad144c0044d57fc";
      });
    }) // {
    package-description-override = "cabal-version: 1.24\nname:          bifunctors\ncategory:      Data, Functors\nversion:       5.6.1\nlicense:       BSD3\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/bifunctors/\nbug-reports:   http://github.com/ekmett/bifunctors/issues\ncopyright:     Copyright (C) 2008-2016 Edward A. Kmett\nsynopsis:      Bifunctors\ndescription:   Bifunctors.\nbuild-type:    Simple\ntested-with:   GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.4\n             , GHC == 8.10.7\n             , GHC == 9.0.2\n             , GHC == 9.2.6\n             , GHC == 9.4.4\n             , GHC == 9.6.1\nextra-source-files:\n  CHANGELOG.markdown\n  README.markdown\n\nsource-repository head\n  type: git\n  location: https://github.com/ekmett/bifunctors.git\n\nflag tagged\n  default: True\n  manual: True\n  description:\n    You can disable the use of the `tagged` package using `-f-tagged`.\n    .\n    Disabing this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n\nlibrary\n  hs-source-dirs: src\n  build-depends:\n    base                     >= 4.9     && < 5,\n    assoc                    >= 1.1     && < 1.2,\n    comonad                  >= 5.0.7   && < 6,\n    containers               >= 0.5.7.1 && < 0.7,\n    template-haskell         >= 2.11    && < 2.21,\n    th-abstraction           >= 0.4.2.0 && < 0.6,\n    transformers             >= 0.5     && < 0.7\n\n  if !impl(ghc >= 8.2)\n    build-depends:\n      bifunctor-classes-compat >= 0.1 && < 0.2,\n      transformers-compat      >= 0.6 && < 0.8\n\n  if flag(tagged)\n    build-depends: tagged >= 0.8.6 && < 1\n\n  if impl(ghc<8.1)\n    reexported-modules:\n        Data.Bifoldable\n      , Data.Bitraversable\n\n  if !impl(ghc >= 9.6)\n    build-depends: foldable1-classes-compat >= 0.1 && < 0.2\n\n  exposed-modules:\n    Data.Biapplicative\n    Data.Bifunctor.Biap\n    Data.Bifunctor.Biff\n    Data.Bifunctor.Clown\n    Data.Bifunctor.Fix\n    Data.Bifunctor.Flip\n    Data.Bifunctor.Functor\n    Data.Bifunctor.Join\n    Data.Bifunctor.Joker\n    Data.Bifunctor.Product\n    Data.Bifunctor.Sum\n    Data.Bifunctor.Tannen\n    Data.Bifunctor.TH\n    Data.Bifunctor.Wrapped\n\n  other-modules:\n    Data.Bifunctor.TH.Internal\n\n  ghc-options: -Wall\n  default-language: Haskell2010\n\n  if impl(ghc >= 9.0)\n    -- these flags may abort compilation with GHC-8.10\n    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3295\n    ghc-options: -Winferred-safe-imports -Wmissing-safe-haskell-mode\n\ntest-suite bifunctors-spec\n  type: exitcode-stdio-1.0\n  hs-source-dirs: tests\n  main-is: Spec.hs\n  other-modules: BifunctorSpec T89Spec\n  ghc-options: -Wall\n  if impl(ghc >= 8.6)\n    ghc-options: -Wno-star-is-type\n  default-language: Haskell2010\n  build-tool-depends: hspec-discover:hspec-discover >= 1.8\n  build-depends:\n    base                >= 4   && < 5,\n    bifunctors,\n    hspec               >= 1.8,\n    QuickCheck          >= 2   && < 3,\n    template-haskell,\n    transformers,\n    transformers-compat\n\n";
    }