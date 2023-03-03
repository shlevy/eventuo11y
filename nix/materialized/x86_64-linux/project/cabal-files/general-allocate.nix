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
      specVersion = "3.0";
      identifier = { name = "general-allocate"; version = "0.2.1.4"; };
      license = "Apache-2.0";
      copyright = "Coypright 2022 Shea Levy.";
      maintainer = "shea@shealevy.com";
      author = "Shea Levy";
      homepage = "";
      url = "";
      synopsis = "Exception-safe resource management in more monads";
      description = "Write monad-generic resource-safe code that can be instantiated in both\nIO and pure contexts.\n\n[exceptions](https://hackage.haskell.org/package/exceptions) provides @MonadMask@,\nwhich generalizes the @bracket@ pattern but only for monads that can catch exceptions\n(i.e., transformer stacks on top of @IO@ or @Either SomeException@). [resourcet](https://hackage.haskell.org/package/resourcet)\nprovides @MonadResource@,\nwhich allows for arbitrarily interleaved allocations and releases of resources, but only\nfor @MonadUnliftIO@. This module provides type-classes for both types of resource allocation\nwhich can be properly instantiated at more monads, through the expedient of weakening a\nrequirement which doesn't matter in the cases where it's not met: In monads that can't catch\nexceptions, release actions will not be called in the case of an exception (asynchronous or\notherwise), but in those same monads the entire monadic computation will be terminated by\nthe same exception and so resource management has ended anyway.\n\nSee \"Control.Monad.With\" for scoped (@bracket@-style) resource management.\n\nSee \"Control.Monad.Allocate\" for arbitrary lifetime resource management.\n\nSee \"Data.GeneralAllocate\" to define values that can be used for allocation in\neither resource management style.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/general-allocate-0.2.1.4.tar.gz";
      sha256 = "e6f66ee429c8ff27fdf15a283b64f087bf3bfce8a3918951e6a3d4f17f4c48b7";
      });
    }) // {
    package-description-override = "cabal-version:      3.0\nname:               general-allocate\nversion:            0.2.1.4\nlicense:            Apache-2.0\nlicense-file:       LICENSE\nauthor:             Shea Levy\nmaintainer:         shea@shealevy.com\ncopyright:          Coypright 2022 Shea Levy.\ncategory:           Control, Exceptions\nextra-source-files: CHANGELOG.md\ntested-with:        GHC == { 8.10.7, 9.2.6 }\nsynopsis:           Exception-safe resource management in more monads\ndescription:\n  Write monad-generic resource-safe code that can be instantiated in both\n  IO and pure contexts.\n\n  [exceptions](https://hackage.haskell.org/package/exceptions) provides @MonadMask@,\n  which generalizes the @bracket@ pattern but only for monads that can catch exceptions\n  (i.e., transformer stacks on top of @IO@ or @Either SomeException@). [resourcet](https://hackage.haskell.org/package/resourcet)\n  provides @MonadResource@,\n  which allows for arbitrarily interleaved allocations and releases of resources, but only\n  for @MonadUnliftIO@. This module provides type-classes for both types of resource allocation\n  which can be properly instantiated at more monads, through the expedient of weakening a\n  requirement which doesn't matter in the cases where it's not met: In monads that can't catch\n  exceptions, release actions will not be called in the case of an exception (asynchronous or\n  otherwise), but in those same monads the entire monadic computation will be terminated by\n  the same exception and so resource management has ended anyway.\n\n  See \"Control.Monad.With\" for scoped (@bracket@-style) resource management.\n\n  See \"Control.Monad.Allocate\" for arbitrary lifetime resource management.\n\n  See \"Data.GeneralAllocate\" to define values that can be used for allocation in\n  either resource management style.\nbug-reports:        https://github.com/shlevy/general-allocate/issues\n\nsource-repository head\n  type:     git\n  location: https://github.com/shlevy/general-allocate\n\nlibrary\n  exposed-modules:\n    Control.Monad.Allocate\n    Control.Monad.NoContinuation.Resource\n    Control.Monad.NoContinuation.Resource.Internal\n    Control.Monad.With\n    Data.GeneralAllocate\n    Data.Exceptable\n\n  build-depends:\n    , base             ^>= { 4.14, 4.16, 4.17 }\n    , containers       ^>= { 0.6 }\n    , mtl              ^>= { 2.2, 2.3 }\n    , primitive        ^>= { 0.7, 0.8 }\n    , resourcet        ^>= { 1.3 }\n    , safe-exceptions  ^>= { 0.1 }\n    , transformers     ^>= { 0.5, 0.6 }\n      -- At transformers >= 0.6, remove the Monad (t m) constraint on MonadAllocate AllocateViaLift\n\n  hs-source-dirs:   src\n  default-language: Haskell2010\n";
    }