{
  description = "An event-oriented observability library";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix }: let
    supportedSystems = [ "x86_64-linux" ];
  in flake-utils.lib.eachSystem supportedSystems (system: let
    pkgs = nixpkgs.legacyPackages.${system}.extend haskell-nix.overlay;

    inherit (pkgs) writeShellScript nix jq coreutils git lib;

    ifExists = p: if builtins.pathExists p then p else null;

    tools = {
      cabal = {
        inherit (project) index-state;
        materialized = ifExists (./nix + "/cabal-materialized-${system}");
      };

      hoogle = {
        inherit (project) index-state;
        materialized = ifExists (./nix + "/hoogle-materialized-${system}");
      };
    };
    project = pkgs.haskell-nix.cabalProject' {
      src = ./.;
      compiler-nix-name = "ghc924";
      shell.tools = tools;
      materialized = ifExists (./nix + "/materialized-${system}");
    };

    flake = project.flake {};

    tools-built = project.tools tools;
  in flake // {
    packages = flake.packages // {
      inherit (project.plan-nix.passthru) generateMaterialized;
      cabalGenerateMaterialized = tools-built.cabal.project.plan-nix.passthru.generateMaterialized;
      hoogleGenerateMaterialized = tools-built.hoogle.project.plan-nix.passthru.generateMaterialized;
    };
    apps = flake.apps // {
      updateAllMaterialized = {
        type = "app";
        program = (writeShellScript "updateAllMaterialized" ''
          set -eEuo pipefail
          export PATH="${lib.makeBinPath [ nix jq coreutils git ]}"
          export NIX_CONFIG="
            allow-import-from-derivation = true
            experimental-features = flakes nix-command
          "
          ${builtins.concatStringsSep "\n" (map (system: ''
            script="$(nix build .#packages.${system}.generateMaterialized --json --no-link | jq -r '.[0].outputs.out')"
            echo "Running $script on ./nix/materialized-${system}" >&2
            "$script" "./nix/materialized-${system}"

            script="$(nix build .#packages.${system}.cabalGenerateMaterialized --json --no-link | jq -r '.[0].outputs.out')"
            echo "Running $script on ./nix/cabal-materialized-${system}" >&2
            "$script" "./nix/cabal-materialized-${system}"

            script="$(nix build .#packages.${system}.hoogleGenerateMaterialized --json --no-link | jq -r '.[0].outputs.out')"
            echo "Running $script on ./nix/hoogle-materialized-${system}" >&2
            "$script" "./nix/hoogle-materialized-${system}"
          '') supportedSystems)}
        '').outPath;
      };
    };
  });
}
