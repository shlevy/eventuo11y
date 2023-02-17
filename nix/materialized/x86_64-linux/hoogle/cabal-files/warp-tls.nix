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
      identifier = { name = "warp-tls"; version = "3.3.5"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "http://github.com/yesodweb/wai";
      url = "";
      synopsis = "HTTP over TLS support for Warp via the TLS package";
      description = "SSLv1 and SSLv2 are obsoleted by IETF.\nWe should use TLS 1.2 (or TLS 1.1 or TLS 1.0 if necessary).\nHTTP/2 can be negotiated by ALPN.\nAPI docs and the README are available at\n<http://www.stackage.org/package/warp-tls>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."tls" or (errorHandler.buildDepError "tls"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."tls-session-manager" or (errorHandler.buildDepError "tls-session-manager"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."recv" or (errorHandler.buildDepError "recv"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/warp-tls-3.3.5.tar.gz";
      sha256 = "87e55aa5688d3d86fec6e23e0796af9d8816956506d5eb13e737cad2dbd7a9f0";
      });
    }) // {
    package-description-override = "Name:                warp-tls\nVersion:             3.3.5\nSynopsis:            HTTP over TLS support for Warp via the TLS package\nLicense:             MIT\nLicense-file:        LICENSE\nAuthor:              Michael Snoyman\nMaintainer:          michael@snoyman.com\nHomepage:            http://github.com/yesodweb/wai\nCategory:            Web, Yesod\nBuild-Type:          Simple\nCabal-Version:       >= 1.10\nStability:           Stable\ndescription:         SSLv1 and SSLv2 are obsoleted by IETF.\n                     We should use TLS 1.2 (or TLS 1.1 or TLS 1.0 if necessary).\n                     HTTP/2 can be negotiated by ALPN.\n                     API docs and the README are available at\n                     <http://www.stackage.org/package/warp-tls>.\nextra-source-files:  ChangeLog.md README.md\n\nLibrary\n  Build-Depends:     base                          >= 4.12     && < 5\n                   , bytestring                    >= 0.9\n                   , wai                           >= 3.2      && < 3.3\n                   , warp                          >= 3.3.23   && < 3.4\n                   , data-default-class            >= 0.0.1\n                   , tls                           >= 1.5.3\n                   , cryptonite                    >= 0.12\n                   , network                       >= 2.2.1\n                   , streaming-commons\n                   , tls-session-manager           >= 0.0.4\n                   , unliftio\n                   , recv                          >= 0.1.0   && < 0.2.0\n  Exposed-modules:   Network.Wai.Handler.WarpTLS\n                     Network.Wai.Handler.WarpTLS.Internal\n  ghc-options:       -Wall\n  if os(windows)\n      Cpp-Options:   -DWINDOWS\n  if impl(ghc >= 8)\n      Default-Extensions:  Strict StrictData\n  Default-Language:     Haskell2010\n\n\nsource-repository head\n  type:     git\n  location: git://github.com/yesodweb/wai.git\n";
    }