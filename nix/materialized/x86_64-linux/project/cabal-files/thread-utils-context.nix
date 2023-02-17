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
      specVersion = "1.12";
      identifier = { name = "thread-utils-context"; version = "0.2.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2021 Ian Duncan";
      maintainer = "ian@iankduncan.com";
      author = "Ian Duncan";
      homepage = "https://github.com/iand675/thread-utils#readme";
      url = "";
      synopsis = "Garbage-collected thread local storage";
      description = "Please see the README on GitHub at <https://github.com/iand675/thread-utils-context#readme>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."thread-utils-finalizers" or (errorHandler.buildDepError "thread-utils-finalizers"))
          ];
        buildable = true;
        };
      tests = {
        "thread-utils-context-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."thread-utils-context" or (errorHandler.buildDepError "thread-utils-context"))
            (hsPkgs."thread-utils-finalizers" or (errorHandler.buildDepError "thread-utils-finalizers"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "thread-utils-context-benchmarks" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."thread-utils-context" or (errorHandler.buildDepError "thread-utils-context"))
            (hsPkgs."thread-utils-finalizers" or (errorHandler.buildDepError "thread-utils-finalizers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/thread-utils-context-0.2.0.0.tar.gz";
      sha256 = "2957c21e331a75be8cf547eed1777f70cd28b28c74552f9a4d22ff51ef4ab92f";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\n\n-- This file has been generated from package.yaml by hpack version 0.34.4.\n--\n-- see: https://github.com/sol/hpack\n\nname:           thread-utils-context\nversion:        0.2.0.0\nsynopsis:       Garbage-collected thread local storage\ndescription:    Please see the README on GitHub at <https://github.com/iand675/thread-utils-context#readme>\ncategory:       Concurrency\nhomepage:       https://github.com/iand675/thread-utils#readme\nbug-reports:    https://github.com/iand675/thread-utils/issues\nauthor:         Ian Duncan\nmaintainer:     ian@iankduncan.com\ncopyright:      2021 Ian Duncan\nlicense:        BSD3\nlicense-file:   LICENSE\nbuild-type:     Simple\nextra-source-files:\n    README.md\n    ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/iand675/thread-utils\n\nlibrary\n  exposed-modules:\n      Control.Concurrent.Thread.Storage\n  other-modules:\n      Paths_thread_utils_context\n  hs-source-dirs:\n      src\n  build-depends:\n      base >=4.7 && <5\n    , containers\n    , ghc-prim\n    , thread-utils-finalizers\n  default-language: Haskell2010\n\ntest-suite thread-utils-context-test\n  type: exitcode-stdio-1.0\n  main-is: Spec.hs\n  other-modules:\n      Paths_thread_utils_context\n  hs-source-dirs:\n      test\n  ghc-options: -threaded -rtsopts -with-rtsopts=-N\n  build-depends:\n      base >=4.7 && <5\n    , containers\n    , ghc-prim\n    , thread-utils-context\n    , thread-utils-finalizers\n  default-language: Haskell2010\n\nbenchmark thread-utils-context-benchmarks\n  type: exitcode-stdio-1.0\n  main-is: Bench.hs\n  other-modules:\n      Paths_thread_utils_context\n  hs-source-dirs:\n      bench\n  ghc-options: -O2 -rtsopts -threaded -with-rtsopts=-N\n  build-depends:\n      base >=4.7 && <5\n    , containers\n    , criterion\n    , ghc-prim\n    , mtl\n    , thread-utils-context\n    , thread-utils-finalizers\n  default-language: Haskell2010\n";
    }