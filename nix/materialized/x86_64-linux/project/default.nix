{
  pkgs = hackage:
    {
      packages = {
        bytestring.revision = (((hackage.bytestring)."0.11.4.0").revisions).default;
        safe-exceptions.revision = import ./cabal-files/safe-exceptions.nix;
        http-client.revision = import ./cabal-files/http-client.nix;
        http-client.flags.network-uri = true;
        attoparsec-iso8601.revision = import ./cabal-files/attoparsec-iso8601.nix;
        safe.revision = import ./cabal-files/safe.nix;
        dlist.revision = import ./cabal-files/dlist.nix;
        dlist.flags.werror = false;
        exceptions.revision = (((hackage.exceptions)."0.10.4").revisions).default;
        directory.revision = (((hackage.directory)."1.3.6.2").revisions).default;
        general-allocate.revision = import ./cabal-files/general-allocate.nix;
        http-api-data.revision = import ./cabal-files/http-api-data.nix;
        http-api-data.flags.use-text-show = false;
        vector-stream.revision = import ./cabal-files/vector-stream.nix;
        filepath.revision = (((hackage.filepath)."1.4.2.2").revisions).default;
        network.revision = import ./cabal-files/network.nix;
        network.flags.devel = false;
        simple-sendfile.revision = import ./cabal-files/simple-sendfile.nix;
        simple-sendfile.flags.allow-bsd = true;
        mtl.revision = (((hackage.mtl)."2.2.2").revisions).default;
        http-types.revision = import ./cabal-files/http-types.nix;
        string-conversions.revision = import ./cabal-files/string-conversions.nix;
        ghc-bignum.revision = (((hackage.ghc-bignum)."1.2").revisions).default;
        zlib.revision = import ./cabal-files/zlib.nix;
        zlib.flags.non-blocking-ffi = false;
        zlib.flags.bundled-c-zlib = false;
        zlib.flags.pkg-config = false;
        adjunctions.revision = import ./cabal-files/adjunctions.nix;
        vault.revision = import ./cabal-files/vault.nix;
        vault.flags.useghc = true;
        singleton-bool.revision = import ./cabal-files/singleton-bool.nix;
        tagged.revision = import ./cabal-files/tagged.nix;
        tagged.flags.deepseq = true;
        tagged.flags.transformers = true;
        cryptohash-sha1.revision = import ./cabal-files/cryptohash-sha1.nix;
        unliftio.revision = import ./cabal-files/unliftio.nix;
        data-fix.revision = import ./cabal-files/data-fix.nix;
        sop-core.revision = import ./cabal-files/sop-core.nix;
        unix-compat.revision = import ./cabal-files/unix-compat.nix;
        unix-compat.flags.old-time = false;
        cryptonite.revision = import ./cabal-files/cryptonite.nix;
        cryptonite.flags.check_alignment = false;
        cryptonite.flags.support_sse = false;
        cryptonite.flags.use_target_attributes = true;
        cryptonite.flags.support_deepseq = true;
        cryptonite.flags.support_rdrand = true;
        cryptonite.flags.old_toolchain_inliner = false;
        cryptonite.flags.integer-gmp = true;
        cryptonite.flags.support_pclmuldq = false;
        cryptonite.flags.support_aesni = true;
        asn1-parse.revision = import ./cabal-files/asn1-parse.nix;
        ghc-prim.revision = (((hackage.ghc-prim)."0.8.0").revisions).default;
        transformers-base.revision = import ./cabal-files/transformers-base.nix;
        transformers-base.flags.orphaninstances = true;
        time-manager.revision = import ./cabal-files/time-manager.nix;
        unliftio-core.revision = import ./cabal-files/unliftio-core.nix;
        memory.revision = import ./cabal-files/memory.nix;
        memory.flags.support_deepseq = true;
        memory.flags.support_bytestring = true;
        constraints.revision = import ./cabal-files/constraints.nix;
        parsec.revision = (((hackage.parsec)."3.1.15.0").revisions).default;
        streaming-commons.revision = import ./cabal-files/streaming-commons.nix;
        streaming-commons.flags.use-bytestring-builder = false;
        x509.revision = import ./cabal-files/x509.nix;
        some.revision = import ./cabal-files/some.nix;
        some.flags.newtype-unsafe = true;
        unix-time.revision = import ./cabal-files/unix-time.nix;
        hourglass.revision = import ./cabal-files/hourglass.nix;
        bifunctors.revision = import ./cabal-files/bifunctors.nix;
        bifunctors.flags.tagged = true;
        bifunctors.flags.semigroups = true;
        integer-logarithms.revision = import ./cabal-files/integer-logarithms.nix;
        integer-logarithms.flags.check-bounds = false;
        integer-logarithms.flags.integer-gmp = true;
        strict.revision = import ./cabal-files/strict.nix;
        strict.flags.assoc = true;
        base-compat.revision = import ./cabal-files/base-compat.nix;
        aeson.revision = import ./cabal-files/aeson.nix;
        aeson.flags.ordered-keymap = true;
        aeson.flags.cffi = false;
        recv.revision = import ./cabal-files/recv.nix;
        utf8-string.revision = import ./cabal-files/utf8-string.nix;
        entropy.revision = import ./cabal-files/entropy.nix;
        entropy.flags.donotgetentropy = false;
        Cabal.revision = (((hackage.Cabal)."3.6.3.0").revisions).default;
        splitmix.revision = import ./cabal-files/splitmix.nix;
        splitmix.flags.optimised-mixer = false;
        assoc.revision = import ./cabal-files/assoc.nix;
        containers.revision = (((hackage.containers)."0.6.5.1").revisions).default;
        http-date.revision = import ./cabal-files/http-date.nix;
        clock.revision = import ./cabal-files/clock.nix;
        clock.flags.llvm = false;
        stm.revision = (((hackage.stm)."2.5.0.2").revisions).default;
        http-media.revision = import ./cabal-files/http-media.nix;
        semigroups.revision = import ./cabal-files/semigroups.nix;
        semigroups.flags.bytestring = true;
        semigroups.flags.bytestring-builder = false;
        semigroups.flags.tagged = true;
        semigroups.flags.containers = true;
        semigroups.flags.deepseq = true;
        semigroups.flags.template-haskell = true;
        semigroups.flags.binary = true;
        semigroups.flags.transformers = true;
        semigroups.flags.unordered-containers = true;
        semigroups.flags.text = true;
        semigroups.flags.hashable = true;
        hs-opentelemetry-api.revision = import ./cabal-files/hs-opentelemetry-api.nix;
        OneTuple.revision = import ./cabal-files/OneTuple.nix;
        network-uri.revision = import ./cabal-files/network-uri.nix;
        base.revision = (((hackage.base)."4.16.4.0").revisions).default;
        time.revision = (((hackage.time)."1.11.1.1").revisions).default;
        pem.revision = import ./cabal-files/pem.nix;
        network-info.revision = import ./cabal-files/network-info.nix;
        uuid.revision = import ./cabal-files/uuid.nix;
        http2.revision = import ./cabal-files/http2.nix;
        http2.flags.devel = false;
        http2.flags.h2spec = false;
        http2.flags.doc = false;
        resourcet.revision = import ./cabal-files/resourcet.nix;
        uuid-types.revision = import ./cabal-files/uuid-types.nix;
        bsb-http-chunked.revision = import ./cabal-files/bsb-http-chunked.nix;
        case-insensitive.revision = import ./cabal-files/case-insensitive.nix;
        cookie.revision = import ./cabal-files/cookie.nix;
        th-abstraction.revision = import ./cabal-files/th-abstraction.nix;
        hsc2hs.revision = import ./cabal-files/hsc2hs.nix;
        hsc2hs.flags.in-ghc-tree = false;
        semigroupoids.revision = import ./cabal-files/semigroupoids.nix;
        semigroupoids.flags.tagged = true;
        semigroupoids.flags.containers = true;
        semigroupoids.flags.distributive = true;
        semigroupoids.flags.unordered-containers = true;
        semigroupoids.flags.contravariant = true;
        semigroupoids.flags.comonad = true;
        free.revision = import ./cabal-files/free.nix;
        network-byte-order.revision = import ./cabal-files/network-byte-order.nix;
        asn1-encoding.revision = import ./cabal-files/asn1-encoding.nix;
        profunctors.revision = import ./cabal-files/profunctors.nix;
        void.revision = import ./cabal-files/void.nix;
        void.flags.safe = false;
        old-locale.revision = import ./cabal-files/old-locale.nix;
        generically.revision = import ./cabal-files/generically.nix;
        async.revision = import ./cabal-files/async.nix;
        async.flags.bench = false;
        word8.revision = import ./cabal-files/word8.nix;
        base-orphans.revision = import ./cabal-files/base-orphans.nix;
        kan-extensions.revision = import ./cabal-files/kan-extensions.nix;
        witherable.revision = import ./cabal-files/witherable.nix;
        random.revision = import ./cabal-files/random.nix;
        iproute.revision = import ./cabal-files/iproute.nix;
        primitive.revision = import ./cabal-files/primitive.nix;
        deepseq.revision = (((hackage.deepseq)."1.4.6.1").revisions).default;
        type-equality.revision = import ./cabal-files/type-equality.nix;
        cryptohash-md5.revision = import ./cabal-files/cryptohash-md5.nix;
        servant-client.revision = import ./cabal-files/servant-client.nix;
        mmorph.revision = import ./cabal-files/mmorph.nix;
        vector-builder.revision = import ./cabal-files/vector-builder.nix;
        distributive.revision = import ./cabal-files/distributive.nix;
        distributive.flags.tagged = true;
        distributive.flags.semigroups = true;
        rts.revision = (((hackage.rts)."1.0.2").revisions).default;
        temporary.revision = import ./cabal-files/temporary.nix;
        template-haskell.revision = (((hackage.template-haskell)."2.18.0.0").revisions).default;
        binary.revision = (((hackage.binary)."0.8.9.0").revisions).default;
        indexed-traversable-instances.revision = import ./cabal-files/indexed-traversable-instances.nix;
        boring.revision = import ./cabal-files/boring.nix;
        boring.flags.tagged = true;
        asn1-types.revision = import ./cabal-files/asn1-types.nix;
        auto-update.revision = import ./cabal-files/auto-update.nix;
        base64-bytestring.revision = import ./cabal-files/base64-bytestring.nix;
        blaze-builder.revision = import ./cabal-files/blaze-builder.nix;
        old-time.revision = import ./cabal-files/old-time.nix;
        invariant.revision = import ./cabal-files/invariant.nix;
        data-default-class.revision = import ./cabal-files/data-default-class.nix;
        text-short.revision = import ./cabal-files/text-short.nix;
        text-short.flags.asserts = false;
        monad-control.revision = import ./cabal-files/monad-control.nix;
        transformers-compat.revision = import ./cabal-files/transformers-compat.nix;
        transformers-compat.flags.two = false;
        transformers-compat.flags.mtl = true;
        transformers-compat.flags.four = false;
        transformers-compat.flags.five = false;
        transformers-compat.flags.five-three = true;
        transformers-compat.flags.three = false;
        transformers-compat.flags.generic-deriving = true;
        integer-gmp.revision = (((hackage.integer-gmp)."1.1").revisions).default;
        time-compat.revision = import ./cabal-files/time-compat.nix;
        time-compat.flags.old-locale = false;
        process.revision = (((hackage.process)."1.6.16.0").revisions).default;
        unix.revision = (((hackage.unix)."2.7.2.2").revisions).default;
        data-array-byte.revision = import ./cabal-files/data-array-byte.nix;
        wai.revision = import ./cabal-files/wai.nix;
        dec.revision = import ./cabal-files/dec.nix;
        byteorder.revision = import ./cabal-files/byteorder.nix;
        transformers.revision = (((hackage.transformers)."0.5.6.2").revisions).default;
        servant.revision = import ./cabal-files/servant.nix;
        indexed-traversable.revision = import ./cabal-files/indexed-traversable.nix;
        these.revision = import ./cabal-files/these.nix;
        these.flags.assoc = true;
        psqueues.revision = import ./cabal-files/psqueues.nix;
        unordered-containers.revision = import ./cabal-files/unordered-containers.nix;
        unordered-containers.flags.debug = false;
        QuickCheck.revision = import ./cabal-files/QuickCheck.nix;
        QuickCheck.flags.old-random = false;
        QuickCheck.flags.templatehaskell = true;
        contravariant.revision = import ./cabal-files/contravariant.nix;
        contravariant.flags.tagged = true;
        contravariant.flags.semigroups = true;
        contravariant.flags.statevar = true;
        appar.revision = import ./cabal-files/appar.nix;
        base-compat-batteries.revision = import ./cabal-files/base-compat-batteries.nix;
        scientific.revision = import ./cabal-files/scientific.nix;
        scientific.flags.bytestring-builder = false;
        scientific.flags.integer-simple = false;
        text.revision = (((hackage.text)."1.2.5.0").revisions).default;
        servant-client-core.revision = import ./cabal-files/servant-client-core.nix;
        thread-utils-context.revision = import ./cabal-files/thread-utils-context.nix;
        StateVar.revision = import ./cabal-files/StateVar.nix;
        th-compat.revision = import ./cabal-files/th-compat.nix;
        attoparsec.revision = import ./cabal-files/attoparsec.nix;
        attoparsec.flags.developer = false;
        array.revision = (((hackage.array)."0.5.4.0").revisions).default;
        basement.revision = import ./cabal-files/basement.nix;
        ghc-boot-th.revision = (((hackage.ghc-boot-th)."9.2.6").revisions).default;
        charset.revision = import ./cabal-files/charset.nix;
        vector.revision = import ./cabal-files/vector.nix;
        vector.flags.internalchecks = false;
        vector.flags.wall = false;
        vector.flags.boundschecks = true;
        vector.flags.unsafechecks = false;
        thread-utils-finalizers.revision = import ./cabal-files/thread-utils-finalizers.nix;
        comonad.revision = import ./cabal-files/comonad.nix;
        comonad.flags.containers = true;
        comonad.flags.distributive = true;
        comonad.flags.indexed-traversable = true;
        semialign.revision = import ./cabal-files/semialign.nix;
        semialign.flags.semigroupoids = true;
        mime-types.revision = import ./cabal-files/mime-types.nix;
        warp.revision = import ./cabal-files/warp.nix;
        warp.flags.network-bytestring = false;
        warp.flags.x509 = true;
        warp.flags.allow-sendfilefd = true;
        warp.flags.warp-debug = false;
        pretty.revision = (((hackage.pretty)."1.1.3.6").revisions).default;
        hashable.revision = import ./cabal-files/hashable.nix;
        hashable.flags.random-initial-seed = false;
        hashable.flags.integer-gmp = true;
        };
      compiler = {
        version = "9.2.6";
        nix-name = "ghc926";
        packages = {
          "pretty" = "1.1.3.6";
          "text" = "1.2.5.0";
          "array" = "0.5.4.0";
          "Cabal" = "3.6.3.0";
          "mtl" = "2.2.2";
          "parsec" = "3.1.15.0";
          "bytestring" = "0.11.4.0";
          "filepath" = "1.4.2.2";
          "stm" = "2.5.0.2";
          "ghc-prim" = "0.8.0";
          "ghc-boot-th" = "9.2.6";
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
        eventuo11y-otel = ./.plan.nix/eventuo11y-otel.nix;
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
          "eventuo11y-otel" = { flags = {}; };
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
          "vector-builder".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "some".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "x509".components.library.planned = lib.mkOverride 900 true;
          "servant-client-core".components.library.planned = lib.mkOverride 900 true;
          "eventuo11y-json".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "charset".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "scientific".components.library.planned = lib.mkOverride 900 true;
          "boring".components.library.planned = lib.mkOverride 900 true;
          "eventuo11y-batteries".components.library.planned = lib.mkOverride 900 true;
          "thread-utils-finalizers".components.library.planned = lib.mkOverride 900 true;
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
          "general-allocate".components.library.planned = lib.mkOverride 900 true;
          "eventuo11y-dsl".components.library.planned = lib.mkOverride 900 true;
          "memory".components.library.planned = lib.mkOverride 900 true;
          "pem".components.library.planned = lib.mkOverride 900 true;
          "base-compat-batteries".components.library.planned = lib.mkOverride 900 true;
          "contravariant".components.library.planned = lib.mkOverride 900 true;
          "appar".components.library.planned = lib.mkOverride 900 true;
          "text-short".components.library.planned = lib.mkOverride 900 true;
          "data-array-byte".components.library.planned = lib.mkOverride 900 true;
          "entropy".components.library.planned = lib.mkOverride 900 true;
          "assoc".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "http-date".components.library.planned = lib.mkOverride 900 true;
          "clock".components.library.planned = lib.mkOverride 900 true;
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
          "hs-opentelemetry-api".components.library.planned = lib.mkOverride 900 true;
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
          "thread-utils-context".components.library.planned = lib.mkOverride 900 true;
          "monad-control".components.library.planned = lib.mkOverride 900 true;
          "streaming-commons".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "time-compat".components.library.planned = lib.mkOverride 900 true;
          "basement".components.library.planned = lib.mkOverride 900 true;
          "aeson".components.library.planned = lib.mkOverride 900 true;
          "hourglass".components.library.planned = lib.mkOverride 900 true;
          "base-compat".components.library.planned = lib.mkOverride 900 true;
          "eventuo11y-otel".components.library.planned = lib.mkOverride 900 true;
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