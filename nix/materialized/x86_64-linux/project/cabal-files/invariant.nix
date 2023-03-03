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
      identifier = { name = "invariant"; version = "0.6.1"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Nicolas Frisby <nicolas.frisby@gmail.com>,\nRyan Scott <ryan.gl.scott@gmail.com>";
      author = "Nicolas Frisby <nicolas.frisby@gmail.com>";
      homepage = "https://github.com/nfrisby/invariant-functors";
      url = "";
      synopsis = "Haskell98 invariant functors";
      description = "Haskell98 invariant functors (also known as exponential functors).\n\nFor more information, see Edward Kmett's article \\\"Rotten Bananas\\\":\n\n<http://comonad.com/reader/2008/rotten-bananas/>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."StateVar" or (errorHandler.buildDepError "StateVar"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."invariant" or (errorHandler.buildDepError "invariant"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
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
      url = "http://hackage.haskell.org/package/invariant-0.6.1.tar.gz";
      sha256 = "fb7294ade6554e9c6e77020f065f3acb63f78e62019e4830ea509c36c8b0d4f0";
      });
    }) // {
    package-description-override = "name:                invariant\nversion:             0.6.1\nsynopsis:            Haskell98 invariant functors\ndescription:         Haskell98 invariant functors (also known as exponential functors).\n                     .\n                     For more information, see Edward Kmett's article \\\"Rotten Bananas\\\":\n                     .\n                     <http://comonad.com/reader/2008/rotten-bananas/>\ncategory:            Control, Data\nlicense:             BSD2\nlicense-file:        LICENSE\nhomepage:            https://github.com/nfrisby/invariant-functors\nbug-reports:         https://github.com/nfrisby/invariant-functors/issues\nauthor:              Nicolas Frisby <nicolas.frisby@gmail.com>\nmaintainer:          Nicolas Frisby <nicolas.frisby@gmail.com>,\n                     Ryan Scott <ryan.gl.scott@gmail.com>\nbuild-type:          Simple\ncabal-version:       >= 1.10\ntested-with:         GHC == 7.0.4\n                   , GHC == 7.2.2\n                   , GHC == 7.4.2\n                   , GHC == 7.6.3\n                   , GHC == 7.8.4\n                   , GHC == 7.10.3\n                   , GHC == 8.0.2\n                   , GHC == 8.2.2\n                   , GHC == 8.4.4\n                   , GHC == 8.6.5\n                   , GHC == 8.8.4\n                   , GHC == 8.10.7\n                   , GHC == 9.0.2\n                   , GHC == 9.2.6\n                   , GHC == 9.4.4\n                   , GHC == 9.6.1\nextra-source-files:  CHANGELOG.md, README.md\n\nsource-repository head\n  type:                git\n  location:            https://github.com/nfrisby/invariant-functors\n\nlibrary\n  exposed-modules:     Data.Functor.Invariant\n                     , Data.Functor.Invariant.TH\n  other-modules:       Data.Functor.Invariant.TH.Internal\n                     , Paths_invariant\n  hs-source-dirs:      src\n  default-language:    Haskell2010\n  build-depends:       array                >= 0.3    && < 0.6\n                     , base                 >= 4      && < 5\n                     , bifunctors           >= 5.2    && < 6\n                     , comonad              >= 5      && < 6\n                     , containers           >= 0.1    && < 0.7\n                     , contravariant        >= 0.5    && < 2\n                     , ghc-prim\n                     , profunctors          >= 5.2.1  && < 6\n                     , StateVar             >= 1.1    && < 2\n                     , stm                  >= 2.2    && < 3\n                     , tagged               >= 0.7.3  && < 1\n                     , template-haskell     >= 2.4    && < 2.21\n                     , th-abstraction       >= 0.4    && < 0.6\n                     , transformers         >= 0.2    && < 0.7\n                     , transformers-compat  >= 0.3    && < 1\n                     , unordered-containers >= 0.2.4  && < 0.3\n  ghc-options:         -Wall\n\n  if !impl(ghc >= 8.0)\n    build-depends:     semigroups           >= 0.16.2 && < 1\n\ntest-suite spec\n  type:                exitcode-stdio-1.0\n  hs-source-dirs:      test\n  default-language:    Haskell2010\n  main-is:             Spec.hs\n  other-modules:       InvariantSpec\n                       THSpec\n  build-depends:       base             >= 4    && < 5\n                     , hspec            >= 1.8\n                     , invariant\n                     , QuickCheck       >= 2.11 && < 3\n                     , template-haskell\n  build-tool-depends:  hspec-discover:hspec-discover\n  ghc-options:         -Wall\n  if impl(ghc >= 8.6)\n    ghc-options:       -Wno-star-is-type\n";
    }