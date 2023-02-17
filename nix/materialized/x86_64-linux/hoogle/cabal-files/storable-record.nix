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
    flags = { splitbase = true; buildtests = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "storable-record"; version = "0.0.6"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Henning Thielemann <storable@henning-thielemann.de>";
      author = "Henning Thielemann <storable@henning-thielemann.de>";
      homepage = "http://code.haskell.org/~thielema/storable-record/";
      url = "";
      synopsis = "Elegant definition of Storable instances for records";
      description = "With this package\nyou can build a Storable instance of a record type\nfrom Storable instances of its elements in an elegant way.\nIt does not do any magic,\njust a bit arithmetic to compute the right offsets,\nthat would be otherwise done manually\nor by a preprocessor like C2HS.\nI cannot promise that the generated memory layout\nis compatible with that of a corresponding C struct.\nHowever, the module generates the smallest layout\nthat is possible with respect to the alignment of the record elements.\nIf you encounter, that a record does not have a compatible layout,\nwe should fix that.\nBut also without C compatibility this package is useful\ne.g. in connection with StorableVector.\n\nWe provide Storable instance support for several cases:\n\n* If you wrap a type in a @newtype@,\nthen you can lift its 'Storable' instance to that @newtype@\nwith the module \"Foreign.Storable.Newtype\".\nThis way you do not need the @GeneralizedNewtypeDeriving@ feature of GHC.\n\n* If you have a type that is an instance of 'Traversable',\nyou can use that feature for implementation of 'Storable' methods.\nThe module \"Foreign.Storable.Traversable\"\nallows manipulation of the portion of your type,\nthat is accessible by 'Traversable' methods.\nFor instance with the type\n@data T a = Cons Int [a]@\nand an according 'Traversable' implementation,\nyou can load and store the elements of the contained list.\nThis may be part of a 'Storable' implementation of the whole type.\n\n* If you have a record containing elements of various types,\nthen you need module \"Foreign.Storable.Record\".\n\nNote however that the Storable instances\ndefined with this package are quite slow in (up to) GHC-6.12.1.\nI'm afraid this is due to incomplete inlining,\nbut we have still to investigate the problem.\n\nFor examples see packages @storable-tuple@ and @sample-frame@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."utility-ht" or (errorHandler.buildDepError "utility-ht"))
          ] ++ (if flags.splitbase
          then [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]
          else [
            (hsPkgs."special-functors" or (errorHandler.buildDepError "special-functors"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ]);
        buildable = true;
        };
      exes = {
        "storable-record-speed" = {
          depends = (pkgs.lib).optionals (flags.buildtests) [
            (hsPkgs."storablevector" or (errorHandler.buildDepError "storablevector"))
            (hsPkgs."timeit" or (errorHandler.buildDepError "timeit"))
            ];
          buildable = if flags.buildtests then true else false;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/storable-record-0.0.6.tar.gz";
      sha256 = "cd09cc2dda10c3addcb6a1f71f964fb33fd8ab4d2b4acd94cd08dfbc180b8cb4";
      });
    }) // {
    package-description-override = "Cabal-Version: 2.2\r\nName:         storable-record\r\nVersion:      0.0.6\r\nx-revision: 1\r\nCategory:     Data, Foreign\r\nSynopsis:     Elegant definition of Storable instances for records\r\nDescription:\r\n  With this package\r\n  you can build a Storable instance of a record type\r\n  from Storable instances of its elements in an elegant way.\r\n  It does not do any magic,\r\n  just a bit arithmetic to compute the right offsets,\r\n  that would be otherwise done manually\r\n  or by a preprocessor like C2HS.\r\n  I cannot promise that the generated memory layout\r\n  is compatible with that of a corresponding C struct.\r\n  However, the module generates the smallest layout\r\n  that is possible with respect to the alignment of the record elements.\r\n  If you encounter, that a record does not have a compatible layout,\r\n  we should fix that.\r\n  But also without C compatibility this package is useful\r\n  e.g. in connection with StorableVector.\r\n  .\r\n  We provide Storable instance support for several cases:\r\n  .\r\n  * If you wrap a type in a @newtype@,\r\n    then you can lift its 'Storable' instance to that @newtype@\r\n    with the module \"Foreign.Storable.Newtype\".\r\n    This way you do not need the @GeneralizedNewtypeDeriving@ feature of GHC.\r\n  .\r\n  * If you have a type that is an instance of 'Traversable',\r\n    you can use that feature for implementation of 'Storable' methods.\r\n    The module \"Foreign.Storable.Traversable\"\r\n    allows manipulation of the portion of your type,\r\n    that is accessible by 'Traversable' methods.\r\n    For instance with the type\r\n    @data T a = Cons Int [a]@\r\n    and an according 'Traversable' implementation,\r\n    you can load and store the elements of the contained list.\r\n    This may be part of a 'Storable' implementation of the whole type.\r\n  .\r\n  * If you have a record containing elements of various types,\r\n    then you need module \"Foreign.Storable.Record\".\r\n  .\r\n  Note however that the Storable instances\r\n  defined with this package are quite slow in (up to) GHC-6.12.1.\r\n  I'm afraid this is due to incomplete inlining,\r\n  but we have still to investigate the problem.\r\n  .\r\n  For examples see packages @storable-tuple@ and @sample-frame@.\r\nLicense:             BSD-3-Clause\r\nLicense-file:        LICENSE\r\nAuthor:              Henning Thielemann <storable@henning-thielemann.de>\r\nMaintainer:          Henning Thielemann <storable@henning-thielemann.de>\r\nHomepage:            http://code.haskell.org/~thielema/storable-record/\r\nStability:           Experimental\r\nBuild-Type:          Simple\r\nTested-With:         GHC==6.8.2, GHC==6.10.4, GHC==6.12.1, GHC==8.0.1\r\n\r\nSource-Repository head\r\n  Type:     darcs\r\n  Location: http://code.haskell.org/~thielema/storable-record/\r\n\r\nSource-Repository this\r\n  Type:     darcs\r\n  Location: http://code.haskell.org/~thielema/storable-record/\r\n  Tag:      0.0.6\r\n\r\nFlag splitBase\r\n  description: Choose the new smaller, split-up base package.\r\n\r\nFlag buildTests\r\n  description: Build speed test\r\n  default:     False\r\n\r\nLibrary\r\n  Build-Depends:\r\n    QuickCheck >=2 && <3,\r\n    transformers >=0.2 && <0.7,\r\n    semigroups >=0.1 && <1.0,\r\n    utility-ht >=0.0.14 && <0.1\r\n  If flag(splitBase)\r\n    Build-Depends:\r\n      base >= 3 && < 6\r\n  Else\r\n    Build-Depends:\r\n      special-functors >= 1.0 && <1.1,\r\n      base >= 1.0 && < 2\r\n\r\n  Default-Language:    Haskell98\r\n  GHC-Options:         -Wall\r\n  Hs-Source-Dirs:      src\r\n\r\n  Exposed-Modules:\r\n    Foreign.Storable.Record\r\n    Foreign.Storable.Record.Tuple\r\n    Foreign.Storable.Newtype\r\n    Foreign.Storable.Traversable\r\n    Foreign.Storable.FixedArray\r\n  Other-Modules:\r\n    Foreign.Storable.RecordMinimalSize\r\n    Foreign.Storable.RecordReaderPtr\r\n    Foreign.Storable.TraversableUnequalSizes\r\n\r\nExecutable storable-record-speed\r\n  If flag(buildTests)\r\n    Build-Depends:\r\n      storablevector >=0.2.7 && <0.3,\r\n      timeit >=1.0 && <1.1\r\n  Else\r\n    Buildable: False\r\n\r\n  Default-Language:    Haskell98\r\n  GHC-Options:         -Wall\r\n  Hs-Source-Dirs:      src\r\n  Main-Is:             SpeedTest.hs\r\n";
    }