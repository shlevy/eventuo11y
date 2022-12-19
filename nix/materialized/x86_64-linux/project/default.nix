{
  pkgs = hackage:
    {
      packages = {
        "old-time".revision = (((hackage."old-time")."1.1.0.3").revisions).default;
        "servant-client".revision = (((hackage."servant-client")."0.19").revisions).default;
        "mmorph".revision = (((hackage."mmorph")."1.2.0").revisions).default;
        "streaming-commons".revision = (((hackage."streaming-commons")."0.2.2.5").revisions).default;
        "streaming-commons".flags.use-bytestring-builder = false;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "network-uri".revision = (((hackage."network-uri")."2.6.4.1").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.19.1").revisions).default;
        "unordered-containers".flags.debug = false;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3.1").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "hourglass".revision = (((hackage."hourglass")."0.2.12").revisions).default;
        "psqueues".revision = (((hackage."psqueues")."0.2.7.3").revisions).default;
        "adjunctions".revision = (((hackage."adjunctions")."4.4.2").revisions).default;
        "dlist".revision = (((hackage."dlist")."1.0").revisions).default;
        "dlist".flags.werror = false;
        "text".revision = (((hackage."text")."1.2.5.0").revisions).default;
        "entropy".revision = (((hackage."entropy")."0.4.1.10").revisions).default;
        "entropy".flags.donotgetentropy = false;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "base64-bytestring".revision = (((hackage."base64-bytestring")."1.2.1.0").revisions).default;
        "network".revision = (((hackage."network")."3.1.2.7").revisions).default;
        "network".flags.devel = false;
        "invariant".revision = (((hackage."invariant")."0.6").revisions).default;
        "asn1-parse".revision = (((hackage."asn1-parse")."0.9.5").revisions).default;
        "cryptonite".revision = (((hackage."cryptonite")."0.30").revisions).default;
        "cryptonite".flags.check_alignment = false;
        "cryptonite".flags.support_sse = false;
        "cryptonite".flags.use_target_attributes = true;
        "cryptonite".flags.support_deepseq = true;
        "cryptonite".flags.support_rdrand = true;
        "cryptonite".flags.old_toolchain_inliner = false;
        "cryptonite".flags.integer-gmp = true;
        "cryptonite".flags.support_pclmuldq = false;
        "cryptonite".flags.support_aesni = true;
        "vector".revision = (((hackage."vector")."0.13.0.0").revisions).default;
        "vector".flags.internalchecks = false;
        "vector".flags.wall = false;
        "vector".flags.boundschecks = true;
        "vector".flags.unsafechecks = false;
        "generically".revision = (((hackage."generically")."0.1").revisions).default;
        "http-types".revision = (((hackage."http-types")."0.12.3").revisions).default;
        "some".revision = (((hackage."some")."1.0.4.1").revisions).default;
        "some".flags.newtype-unsafe = true;
        "temporary".revision = (((hackage."temporary")."1.3").revisions).default;
        "comonad".revision = (((hackage."comonad")."5.0.8").revisions).default;
        "comonad".flags.containers = true;
        "comonad".flags.distributive = true;
        "comonad".flags.indexed-traversable = true;
        "x509".revision = (((hackage."x509")."1.7.7").revisions).default;
        "profunctors".revision = (((hackage."profunctors")."5.6.2").revisions).default;
        "appar".revision = (((hackage."appar")."0.1.8").revisions).default;
        "asn1-types".revision = (((hackage."asn1-types")."0.3.4").revisions).default;
        "base-compat".revision = (((hackage."base-compat")."0.12.2").revisions).default;
        "string-conversions".revision = (((hackage."string-conversions")."0.4.0.1").revisions).default;
        "contravariant".revision = (((hackage."contravariant")."1.5.5").revisions).default;
        "contravariant".flags.tagged = true;
        "contravariant".flags.semigroups = true;
        "contravariant".flags.statevar = true;
        "base-compat-batteries".revision = (((hackage."base-compat-batteries")."0.12.2").revisions).default;
        "safe".revision = (((hackage."safe")."0.3.19").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."3.6.3.0").revisions).default;
        "sop-core".revision = (((hackage."sop-core")."0.5.0.2").revisions).default;
        "assoc".revision = (((hackage."assoc")."1.0.2").revisions).default;
        "unliftio".revision = (((hackage."unliftio")."0.2.23.0").revisions).default;
        "data-fix".revision = (((hackage."data-fix")."0.3.2").revisions).default;
        "basement".revision = (((hackage."basement")."0.0.15").revisions).default;
        "old-locale".revision = (((hackage."old-locale")."1.0.0.7").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "OneTuple".revision = (((hackage."OneTuple")."0.3.1").revisions).default;
        "mime-types".revision = (((hackage."mime-types")."0.1.1.0").revisions).default;
        "parsec".revision = (((hackage."parsec")."3.1.15.0").revisions).default;
        "bytestring".revision = (((hackage."bytestring")."0.11.3.1").revisions).default;
        "attoparsec-iso8601".revision = (((hackage."attoparsec-iso8601")."1.1.0.0").revisions).default;
        "pem".revision = (((hackage."pem")."0.2.4").revisions).default;
        "strict".revision = (((hackage."strict")."0.4.0.1").revisions).default;
        "strict".flags.assoc = true;
        "aeson".revision = (((hackage."aeson")."2.1.1.0").revisions).default;
        "aeson".flags.ordered-keymap = true;
        "aeson".flags.cffi = false;
        "zlib".revision = (((hackage."zlib")."0.6.3.0").revisions).default;
        "zlib".flags.non-blocking-ffi = false;
        "zlib".flags.bundled-c-zlib = false;
        "zlib".flags.pkg-config = false;
        "tagged".revision = (((hackage."tagged")."0.8.6.1").revisions).default;
        "tagged".flags.deepseq = true;
        "tagged".flags.transformers = true;
        "splitmix".revision = (((hackage."splitmix")."0.1.0.4").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "recv".revision = (((hackage."recv")."0.0.0").revisions).default;
        "attoparsec".revision = (((hackage."attoparsec")."0.14.4").revisions).default;
        "attoparsec".flags.developer = false;
        "singleton-bool".revision = (((hackage."singleton-bool")."0.1.6").revisions).default;
        "th-compat".revision = (((hackage."th-compat")."0.1.4").revisions).default;
        "memory".revision = (((hackage."memory")."0.18.0").revisions).default;
        "memory".flags.support_deepseq = true;
        "memory".flags.support_bytestring = true;
        "filepath".revision = (((hackage."filepath")."1.4.2.2").revisions).default;
        "dec".revision = (((hackage."dec")."0.0.5").revisions).default;
        "unliftio-core".revision = (((hackage."unliftio-core")."0.2.0.1").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.2").revisions).default;
        "void".revision = (((hackage."void")."0.7.3").revisions).default;
        "void".flags.safe = false;
        "semigroups".revision = (((hackage."semigroups")."0.20").revisions).default;
        "semigroups".flags.bytestring = true;
        "semigroups".flags.bytestring-builder = false;
        "semigroups".flags.tagged = true;
        "semigroups".flags.containers = true;
        "semigroups".flags.deepseq = true;
        "semigroups".flags.template-haskell = true;
        "semigroups".flags.binary = true;
        "semigroups".flags.transformers = true;
        "semigroups".flags.unordered-containers = true;
        "semigroups".flags.text = true;
        "semigroups".flags.hashable = true;
        "free".revision = (((hackage."free")."5.1.10").revisions).default;
        "resourcet".revision = (((hackage."resourcet")."1.3.0").revisions).default;
        "vault".revision = (((hackage."vault")."0.3.1.5").revisions).default;
        "vault".flags.useghc = true;
        "unix-compat".revision = (((hackage."unix-compat")."0.6").revisions).default;
        "unix-compat".flags.old-time = false;
        "case-insensitive".revision = (((hackage."case-insensitive")."1.2.1.0").revisions).default;
        "http-date".revision = (((hackage."http-date")."0.0.11").revisions).default;
        "type-equality".revision = (((hackage."type-equality")."1").revisions).default;
        "cryptohash-sha1".revision = (((hackage."cryptohash-sha1")."0.11.101.0").revisions).default;
        "cryptohash-md5".revision = (((hackage."cryptohash-md5")."0.11.101.0").revisions).default;
        "http-media".revision = (((hackage."http-media")."0.8.0.0").revisions).default;
        "time-manager".revision = (((hackage."time-manager")."0.0.0").revisions).default;
        "byteorder".revision = (((hackage."byteorder")."1.0.4").revisions).default;
        "http-api-data".revision = (((hackage."http-api-data")."0.5").revisions).default;
        "http-api-data".flags.use-text-show = false;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.8.0").revisions).default;
        "vector-stream".revision = (((hackage."vector-stream")."0.1.0.0").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."9.2.5").revisions).default;
        "asn1-encoding".revision = (((hackage."asn1-encoding")."0.9.6").revisions).default;
        "indexed-traversable".revision = (((hackage."indexed-traversable")."0.1.2").revisions).default;
        "distributive".revision = (((hackage."distributive")."0.6.2.1").revisions).default;
        "distributive".flags.tagged = true;
        "distributive".flags.semigroups = true;
        "text-short".revision = (((hackage."text-short")."0.1.5").revisions).default;
        "text-short".flags.asserts = false;
        "servant".revision = (((hackage."servant")."0.19.1").revisions).default;
        "bsb-http-chunked".revision = (((hackage."bsb-http-chunked")."0.0.0.4").revisions).default;
        "bifunctors".revision = (((hackage."bifunctors")."5.5.14").revisions).default;
        "bifunctors".flags.tagged = true;
        "bifunctors".flags.semigroups = true;
        "kan-extensions".revision = (((hackage."kan-extensions")."5.2.5").revisions).default;
        "uuid".revision = (((hackage."uuid")."1.3.15").revisions).default;
        "base".revision = (((hackage."base")."4.16.4.0").revisions).default;
        "time".revision = (((hackage."time")."1.11.1.1").revisions).default;
        "network-info".revision = (((hackage."network-info")."0.2.1").revisions).default;
        "async".revision = (((hackage."async")."2.2.4").revisions).default;
        "async".flags.bench = false;
        "word8".revision = (((hackage."word8")."0.1.3").revisions).default;
        "random".revision = (((hackage."random")."1.2.1.1").revisions).default;
        "unix-time".revision = (((hackage."unix-time")."0.4.8").revisions).default;
        "wai".revision = (((hackage."wai")."3.2.3").revisions).default;
        "cookie".revision = (((hackage."cookie")."0.4.5").revisions).default;
        "process".revision = (((hackage."process")."1.6.16.0").revisions).default;
        "utf8-string".revision = (((hackage."utf8-string")."1.0.2").revisions).default;
        "transformers-base".revision = (((hackage."transformers-base")."0.4.6").revisions).default;
        "transformers-base".flags.orphaninstances = true;
        "data-default-class".revision = (((hackage."data-default-class")."0.1.2.0").revisions).default;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.4.5.0").revisions).default;
        "semigroupoids".revision = (((hackage."semigroupoids")."5.3.7").revisions).default;
        "semigroupoids".flags.tagged = true;
        "semigroupoids".flags.containers = true;
        "semigroupoids".flags.distributive = true;
        "semigroupoids".flags.unordered-containers = true;
        "semigroupoids".flags.contravariant = true;
        "semigroupoids".flags.comonad = true;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.8").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "these".revision = (((hackage."these")."1.1.1.1").revisions).default;
        "these".flags.assoc = true;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.7").revisions).default;
        "ghc-bignum".revision = (((hackage."ghc-bignum")."1.2").revisions).default;
        "time-compat".revision = (((hackage."time-compat")."1.9.6.1").revisions).default;
        "time-compat".flags.old-locale = false;
        "http2".revision = (((hackage."http2")."3.0.3").revisions).default;
        "http2".flags.devel = false;
        "http2".flags.h2spec = false;
        "http2".flags.doc = false;
        "primitive".revision = (((hackage."primitive")."0.7.4.0").revisions).default;
        "servant-client-core".revision = (((hackage."servant-client-core")."0.19").revisions).default;
        "directory".revision = (((hackage."directory")."1.3.6.2").revisions).default;
        "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
        "constraints".revision = (((hackage."constraints")."0.13.4").revisions).default;
        "simple-sendfile".revision = (((hackage."simple-sendfile")."0.2.30").revisions).default;
        "simple-sendfile".flags.allow-bsd = true;
        "rts".revision = (((hackage."rts")."1.0.2").revisions).default;
        "network-byte-order".revision = (((hackage."network-byte-order")."0.1.6").revisions).default;
        "monad-control".revision = (((hackage."monad-control")."1.0.3.1").revisions).default;
        "semialign".revision = (((hackage."semialign")."1.2.0.1").revisions).default;
        "semialign".flags.semigroupoids = true;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.18.0.0").revisions).default;
        "witherable".revision = (((hackage."witherable")."0.4.2").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.6.1").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "blaze-builder".revision = (((hackage."blaze-builder")."0.4.2.2").revisions).default;
        "warp".revision = (((hackage."warp")."3.3.23").revisions).default;
        "warp".flags.network-bytestring = false;
        "warp".flags.x509 = true;
        "warp".flags.allow-sendfilefd = true;
        "warp".flags.warp-debug = false;
        "hashable".revision = (((hackage."hashable")."1.4.1.0").revisions).default;
        "hashable".flags.containers = true;
        "hashable".flags.random-initial-seed = false;
        "hashable".flags.integer-gmp = true;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.1").revisions).default;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.7.2").revisions).default;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.mtl = true;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.three = false;
        "transformers-compat".flags.generic-deriving = true;
        "indexed-traversable-instances".revision = (((hackage."indexed-traversable-instances")."0.1.1.1").revisions).default;
        "iproute".revision = (((hackage."iproute")."1.7.12").revisions).default;
        "scientific".revision = (((hackage."scientific")."0.3.7.0").revisions).default;
        "scientific".flags.bytestring-builder = false;
        "scientific".flags.integer-simple = false;
        "binary".revision = (((hackage."binary")."0.8.9.0").revisions).default;
        "http-client".revision = (((hackage."http-client")."0.7.13.1").revisions).default;
        "http-client".flags.network-uri = true;
        "boring".revision = (((hackage."boring")."0.2").revisions).default;
        "boring".flags.tagged = true;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.14.2").revisions).default;
        "QuickCheck".flags.old-random = false;
        "QuickCheck".flags.templatehaskell = true;
        "safe-exceptions".revision = (((hackage."safe-exceptions")."0.1.7.3").revisions).default;
        "uuid-types".revision = (((hackage."uuid-types")."1.0.5").revisions).default;
        "auto-update".revision = (((hackage."auto-update")."0.1.6").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.5.1").revisions).default;
        "StateVar".revision = (((hackage."StateVar")."1.2.2").revisions).default;
        };
      compiler = {
        version = "9.2.5";
        nix-name = "ghc925";
        packages = {
          "pretty" = "1.1.3.6";
          "text" = "1.2.5.0";
          "array" = "0.5.4.0";
          "Cabal" = "3.6.3.0";
          "mtl" = "2.2.2";
          "parsec" = "3.1.15.0";
          "bytestring" = "0.11.3.1";
          "filepath" = "1.4.2.2";
          "stm" = "2.5.0.2";
          "ghc-prim" = "0.8.0";
          "ghc-boot-th" = "9.2.5";
          "base" = "4.16.4.0";
          "time" = "1.11.1.1";
          "process" = "1.6.16.0";
          "ghc-bignum" = "1.2";
          "directory" = "1.3.6.2";
          "exceptions" = "0.10.4";
          "rts" = "1.0.2";
          "transformers" = "0.5.6.2";
          "template-haskell" = "2.18.0.0";
          "deepseq" = "1.4.6.1";
          "unix" = "2.7.2.2";
          "integer-gmp" = "1.1";
          "binary" = "0.8.9.0";
          "containers" = "0.6.5.1";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        eventuo11y = ./.plan.nix/eventuo11y.nix;
        eventuo11y-batteries = ./.plan.nix/eventuo11y-batteries.nix;
        eventuo11y-dsl = ./.plan.nix/eventuo11y-dsl.nix;
        eventuo11y-json = ./.plan.nix/eventuo11y-json.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "eventuo11y" = { flags = {}; };
          "eventuo11y-batteries" = { flags = {}; };
          "eventuo11y-dsl" = { flags = {}; };
          "eventuo11y-json" = { flags = {}; };
          };
        })
    ({ lib, ... }:
      {
        packages = {
          "string-conversions".components.library.planned = lib.mkOverride 900 true;
          "invariant".components.library.planned = lib.mkOverride 900 true;
          "transformers-base".components.library.planned = lib.mkOverride 900 true;
          "base-orphans".components.library.planned = lib.mkOverride 900 true;
          "servant".components.library.planned = lib.mkOverride 900 true;
          "cookie".components.library.planned = lib.mkOverride 900 true;
          "these".components.library.planned = lib.mkOverride 900 true;
          "resourcet".components.library.planned = lib.mkOverride 900 true;
          "http2".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "wai".components.library.planned = lib.mkOverride 900 true;
          "distributive".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "utf8-string".components.library.planned = lib.mkOverride 900 true;
          "Cabal".components.library.planned = lib.mkOverride 900 true;
          "attoparsec-iso8601".components.library.planned = lib.mkOverride 900 true;
          "dec".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "zlib".components.library.planned = lib.mkOverride 900 true;
          "strict".components.library.planned = lib.mkOverride 900 true;
          "entropy".components.setup.planned = lib.mkOverride 900 true;
          "comonad".components.library.planned = lib.mkOverride 900 true;
          "data-fix".components.library.planned = lib.mkOverride 900 true;
          "unliftio".components.library.planned = lib.mkOverride 900 true;
          "sop-core".components.library.planned = lib.mkOverride 900 true;
          "profunctors".components.library.planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "mmorph".components.library.planned = lib.mkOverride 900 true;
          "safe-exceptions".components.library.planned = lib.mkOverride 900 true;
          "old-time".components.library.planned = lib.mkOverride 900 true;
          "dlist".components.library.planned = lib.mkOverride 900 true;
          "time-manager".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "some".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "x509".components.library.planned = lib.mkOverride 900 true;
          "servant-client-core".components.library.planned = lib.mkOverride 900 true;
          "eventuo11y-json".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "scientific".components.library.planned = lib.mkOverride 900 true;
          "boring".components.library.planned = lib.mkOverride 900 true;
          "eventuo11y-batteries".components.library.planned = lib.mkOverride 900 true;
          "splitmix".components.library.planned = lib.mkOverride 900 true;
          "rts".components.library.planned = lib.mkOverride 900 true;
          "temporary".components.library.planned = lib.mkOverride 900 true;
          "tagged".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "simple-sendfile".components.library.planned = lib.mkOverride 900 true;
          "recv".components.library.planned = lib.mkOverride 900 true;
          "hsc2hs".components.exes."hsc2hs".planned = lib.mkOverride 900 true;
          "vector".components.library.planned = lib.mkOverride 900 true;
          "indexed-traversable-instances".components.library.planned = lib.mkOverride 900 true;
          "data-default-class".components.library.planned = lib.mkOverride 900 true;
          "adjunctions".components.library.planned = lib.mkOverride 900 true;
          "cryptonite".components.library.planned = lib.mkOverride 900 true;
          "asn1-parse".components.library.planned = lib.mkOverride 900 true;
          "type-equality".components.library.planned = lib.mkOverride 900 true;
          "network-byte-order".components.library.planned = lib.mkOverride 900 true;
          "mime-types".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "network-info".components.library.planned = lib.mkOverride 900 true;
          "uuid".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "http-api-data".components.library.planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "psqueues".components.library.planned = lib.mkOverride 900 true;
          "bsb-http-chunked".components.library.planned = lib.mkOverride 900 true;
          "StateVar".components.library.planned = lib.mkOverride 900 true;
          "case-insensitive".components.library.planned = lib.mkOverride 900 true;
          "unix-time".components.library.planned = lib.mkOverride 900 true;
          "cryptohash-sha1".components.library.planned = lib.mkOverride 900 true;
          "free".components.library.planned = lib.mkOverride 900 true;
          "unix-compat".components.library.planned = lib.mkOverride 900 true;
          "blaze-builder".components.library.planned = lib.mkOverride 900 true;
          "asn1-types".components.library.planned = lib.mkOverride 900 true;
          "unliftio-core".components.library.planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "safe".components.library.planned = lib.mkOverride 900 true;
          "constraints".components.library.planned = lib.mkOverride 900 true;
          "indexed-traversable".components.library.planned = lib.mkOverride 900 true;
          "network-uri".components.library.planned = lib.mkOverride 900 true;
          "eventuo11y-dsl".components.library.planned = lib.mkOverride 900 true;
          "memory".components.library.planned = lib.mkOverride 900 true;
          "pem".components.library.planned = lib.mkOverride 900 true;
          "base-compat-batteries".components.library.planned = lib.mkOverride 900 true;
          "contravariant".components.library.planned = lib.mkOverride 900 true;
          "appar".components.library.planned = lib.mkOverride 900 true;
          "text-short".components.library.planned = lib.mkOverride 900 true;
          "entropy".components.library.planned = lib.mkOverride 900 true;
          "assoc".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "http-date".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "byteorder".components.library.planned = lib.mkOverride 900 true;
          "witherable".components.library.planned = lib.mkOverride 900 true;
          "generically".components.library.planned = lib.mkOverride 900 true;
          "asn1-encoding".components.library.planned = lib.mkOverride 900 true;
          "semialign".components.library.planned = lib.mkOverride 900 true;
          "http-client".components.library.planned = lib.mkOverride 900 true;
          "async".components.library.planned = lib.mkOverride 900 true;
          "http-media".components.library.planned = lib.mkOverride 900 true;
          "word8".components.library.planned = lib.mkOverride 900 true;
          "iproute".components.library.planned = lib.mkOverride 900 true;
          "servant-client".components.library.planned = lib.mkOverride 900 true;
          "th-compat".components.library.planned = lib.mkOverride 900 true;
          "http-types".components.library.planned = lib.mkOverride 900 true;
          "QuickCheck".components.library.planned = lib.mkOverride 900 true;
          "uuid-types".components.library.planned = lib.mkOverride 900 true;
          "semigroupoids".components.library.planned = lib.mkOverride 900 true;
          "singleton-bool".components.library.planned = lib.mkOverride 900 true;
          "attoparsec".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "vault".components.library.planned = lib.mkOverride 900 true;
          "th-abstraction".components.library.planned = lib.mkOverride 900 true;
          "attoparsec".components.sublibs."attoparsec-internal".planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "OneTuple".components.library.planned = lib.mkOverride 900 true;
          "parsec".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "primitive".components.library.planned = lib.mkOverride 900 true;
          "old-locale".components.library.planned = lib.mkOverride 900 true;
          "eventuo11y".components.library.planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "bifunctors".components.library.planned = lib.mkOverride 900 true;
          "unordered-containers".components.library.planned = lib.mkOverride 900 true;
          "random".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "vector-stream".components.library.planned = lib.mkOverride 900 true;
          "kan-extensions".components.library.planned = lib.mkOverride 900 true;
          "integer-logarithms".components.library.planned = lib.mkOverride 900 true;
          "cryptohash-md5".components.library.planned = lib.mkOverride 900 true;
          "integer-gmp".components.library.planned = lib.mkOverride 900 true;
          "transformers-compat".components.library.planned = lib.mkOverride 900 true;
          "monad-control".components.library.planned = lib.mkOverride 900 true;
          "streaming-commons".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "time-compat".components.library.planned = lib.mkOverride 900 true;
          "basement".components.library.planned = lib.mkOverride 900 true;
          "aeson".components.library.planned = lib.mkOverride 900 true;
          "hourglass".components.library.planned = lib.mkOverride 900 true;
          "base-compat".components.library.planned = lib.mkOverride 900 true;
          "base64-bytestring".components.library.planned = lib.mkOverride 900 true;
          "hashable".components.library.planned = lib.mkOverride 900 true;
          "semigroups".components.library.planned = lib.mkOverride 900 true;
          "auto-update".components.library.planned = lib.mkOverride 900 true;
          "warp".components.library.planned = lib.mkOverride 900 true;
          "void".components.library.planned = lib.mkOverride 900 true;
          };
        })
    ];
  }