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
    flags = { splitbase = true; };
    package = {
      specVersion = "1.6";
      identifier = { name = "storable-tuple"; version = "0.0.3.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Henning Thielemann <storable@henning-thielemann.de>";
      author = "Henning Thielemann <storable@henning-thielemann.de>";
      homepage = "http://code.haskell.org/~thielema/storable-tuple/";
      url = "";
      synopsis = "Storable instance for pairs and triples";
      description = "Provides a Storable instance for pair and triple\nwhich should be binary compatible with C99 and C++.\nThe only purpose of this package is to provide a standard location\nfor this instance so that other packages needing this instance\ncan play nicely together.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."storable-record" or (errorHandler.buildDepError "storable-record"))
          (hsPkgs."utility-ht" or (errorHandler.buildDepError "utility-ht"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          ] ++ (if flags.splitbase
          then [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]
          else [
            (hsPkgs."special-functors" or (errorHandler.buildDepError "special-functors"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ]);
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/storable-tuple-0.0.3.3.tar.gz";
      sha256 = "dcfac049527a45c386c80a7c40ec211455b83d74311af88fa686063b5f87df35";
      });
    }) // {
    package-description-override = "Name:         storable-tuple\nVersion:      0.0.3.3\nCategory:     Data, Foreign\nSynopsis:     Storable instance for pairs and triples\nDescription:\n  Provides a Storable instance for pair and triple\n  which should be binary compatible with C99 and C++.\n\n  The only purpose of this package is to provide a standard location\n  for this instance so that other packages needing this instance\n  can play nicely together.\nLicense:             BSD3\nLicense-file:        LICENSE\nAuthor:              Henning Thielemann <storable@henning-thielemann.de>\nMaintainer:          Henning Thielemann <storable@henning-thielemann.de>\nHomepage:            http://code.haskell.org/~thielema/storable-tuple/\nStability:           Experimental\nBuild-Type:          Simple\nTested-With:         GHC==6.8.2, GHC==8.0.1\nCabal-Version:       >=1.6\n\nSource-Repository head\n  Type:     darcs\n  Location: http://code.haskell.org/~thielema/storable-tuple/\n\nSource-Repository this\n  Type:     darcs\n  Location: http://code.haskell.org/~thielema/storable-tuple/\n  Tag:      0.0.3.3\n\nFlag splitBase\n  Description: Choose the new smaller, split-up base package.\n\nLibrary\n  Build-Depends:\n    storable-record >=0.0.1 && <0.1,\n    utility-ht >=0.0.1 && <0.1,\n    base-orphans >= 0.5 && <1\n  If flag(splitBase)\n    Build-Depends: base >=3 && <5\n  Else\n    Build-Depends:\n      special-functors >= 1.0 && <1.1,\n      base >= 1.0 && < 2\n\n  GHC-Options:         -Wall -fno-warn-orphans\n  Hs-Source-Dirs:      src\n\n  Exposed-Modules:\n    Foreign.Storable.Tuple\n";
    }