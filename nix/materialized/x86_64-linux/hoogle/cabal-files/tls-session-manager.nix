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
      identifier = { name = "tls-session-manager"; version = "0.0.4"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "kazu@iij.ad.jp";
      author = "Kazu Yamamoto";
      homepage = "";
      url = "";
      synopsis = "In-memory TLS session manager";
      description = "TLS session manager with limitation, automatic pruning, energy saving and replay resistance";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."clock" or (errorHandler.buildDepError "clock"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."psqueues" or (errorHandler.buildDepError "psqueues"))
          (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/tls-session-manager-0.0.4.tar.gz";
      sha256 = "ba207f79b4536a65625063106c621e8bafc0dc7928bd9273270e19f36d59938c";
      });
    }) // {
    package-description-override = "name:                tls-session-manager\nversion:             0.0.4\nsynopsis:            In-memory TLS session manager\ndescription:         TLS session manager with limitation, automatic pruning, energy saving and replay resistance\nlicense:             BSD3\nlicense-file:        LICENSE\nauthor:              Kazu Yamamoto\nmaintainer:          kazu@iij.ad.jp\n-- copyright:\ncategory:            Web\nbuild-type:          Simple\nextra-source-files:  ChangeLog.md\ncabal-version:       >= 1.10\n\nlibrary\n  exposed-modules:     Network.TLS.SessionManager\n  other-modules:       Network.TLS.Imports\n  -- other-extensions:\n  build-depends:       base >= 4.7 && < 5\n                     , auto-update\n                     , basement\n                     , bytestring\n                     , clock\n                     , memory\n                     , psqueues >= 0.2.3\n                     , tls\n  -- hs-source-dirs:\n  default-language:    Haskell2010\n  ghc-options:       -Wall\n  if impl(ghc >= 8)\n      default-extensions:  Strict StrictData\n";
    }