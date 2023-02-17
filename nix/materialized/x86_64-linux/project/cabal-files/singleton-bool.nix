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
      identifier = { name = "singleton-bool"; version = "0.1.6"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "https://github.com/phadej/singleton-bool#readme";
      url = "";
      synopsis = "Type level booleans";
      description = "Type level booleans.\n\n@singletons@ package provides similar functionality,\nbut it has tight dependency constraints.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."boring" or (errorHandler.buildDepError "boring"))
          (hsPkgs."dec" or (errorHandler.buildDepError "dec"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."some" or (errorHandler.buildDepError "some"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/singleton-bool-0.1.6.tar.gz";
      sha256 = "5ca3f4802ba0dd89d1817e78f7fbf6900fb5f176f10fc00bdfe395fe572383dd";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               singleton-bool\nversion:            0.1.6\nx-revision:         2\nsynopsis:           Type level booleans\ndescription:\n  Type level booleans.\n  .\n  @singletons@ package provides similar functionality,\n  but it has tight dependency constraints.\n\ncategory:           Web\nhomepage:           https://github.com/phadej/singleton-bool#readme\nbug-reports:        https://github.com/phadej/singleton-bool/issues\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nlicense:            BSD3\nlicense-file:       LICENSE\nbuild-type:         Simple\nextra-source-files: CHANGELOG.md\ntested-with:\n  GHC ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.4\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.1\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/singleton-bool\n\nlibrary\n  hs-source-dirs:   src\n  ghc-options:      -Wall\n  build-depends:\n      base     >=4.7   && <4.18\n    , boring   >=0.2   && <0.3\n    , dec      >=0.0.3 && <0.1\n    , deepseq  >=1.3   && <1.5\n    , some     >=1.0.3 && <1.1\n\n  exposed-modules:  Data.Singletons.Bool\n  default-language: Haskell2010\n";
    }