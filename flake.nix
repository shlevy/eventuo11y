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

    inherit (pkgs) writeShellScript nix_2_5 jq coreutils git lib;

    project = pkgs.haskell-nix.cabalProject' {
      src = ./.;
      compiler-nix-name = "ghc921";
      shell.tools.cabal = {};
      materialized = let materialized = ./nix + "/materialized-${system}"; in if builtins.pathExists materialized then materialized else null;
    };

    flake = project.flake {};
  in flake // {
    packages = flake.packages // {
      inherit (project.plan-nix.passthru) generateMaterialized;
    };
    apps = flake.apps // {
      updateAllMaterialized = {
        type = "app";
        program = (writeShellScript "updateAllMaterialized" ''
          set -eEuo pipefail
          export PATH="${lib.makeBinPath [ nix_2_5 jq coreutils git ]}"
          export NIX_CONFIG="
            allow-import-from-derivation = true
            experimental-features = flakes nix-command
          "
          ${builtins.concatStringsSep "\n" (map (system: ''
            script="$(nix build .#packages.${system}.generateMaterialized --json --no-link | jq -r '.[0].outputs.out')"
            echo "Running $script on ./nix/materialized-${system}" >&2
            "$script" "./nix/materialized-${system}"
          '') supportedSystems)}
        '').outPath;
      };
    };
  });
}
