{
  description = "An event-oriented observability library";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskell-nix,
  } @ inputs: let
    defaultPackageName = "eventuo11y:lib:eventuo11y";
    extraShellPackages = pkgs: with pkgs; [treefmt alejandra ormolu haskellPackages.cabal-fmt];
    supportedSystems = ["x86_64-linux"];

    ifExists = p:
      if builtins.pathExists p
      then p
      else null;

    flake = {
      self,
      nixpkgs,
      flake-utils,
      haskell-nix,
    }:
      flake-utils.lib.eachSystem supportedSystems (evalSystem: let
        packagesBySystem = builtins.listToAttrs (map (system: {
            name = system;

            value = let
              materializedRelative = "/nix/materialized/${system}";

              materializedFor = component: ifExists (./. + materializedRelative + "/${component}");

              pkgs = import nixpkgs {
                inherit system;
                overlays = [haskell-nix.overlay];
                inherit (haskell-nix) config;
              };

              tools = {
                cabal = {
                  inherit (project) index-state evalSystem;
                  version = "3.8.1.0";
                  materialized = materializedFor "cabal";
                };
                hoogle = {
                  inherit (project) index-state evalSystem;
                  version = "5.0.18.3";
                  materialized = materializedFor "hoogle";
                };
              };

              project = pkgs.haskell-nix.cabalProject' {
                inherit evalSystem;
                src = ./.;
                compiler-nix-name = "ghc924";
                shell.tools = tools;
                shell.nativeBuildInputs = extraShellPackages pkgs;
                materialized = materializedFor "project";
              };

              tools-built = project.tools tools;
            in {
              inherit pkgs project;

              update-all-materialized = evalPkgs.writeShellScript "update-all-materialized-${system}" ''
                set -eEuo pipefail
                mkdir -p .${materializedRelative}
                cd .${materializedRelative}
                echo "Updating project materialization" >&2
                ${project.plan-nix.passthru.generateMaterialized} project
                echo "Updating cabal materialization" >&2
                ${tools-built.cabal.project.plan-nix.passthru.generateMaterialized} cabal
                echo "Updating hoogle materialization" >&2
                ${tools-built.hoogle.project.plan-nix.passthru.generateMaterialized} hoogle
              '';
            };
          })
          supportedSystems);

        inherit (packagesBySystem.${evalSystem}) project pkgs;

        evalPkgs = pkgs;

        flake = project.flake {};
      in
        flake
        // rec {
          defaultPackage = packages.default;

          packages =
            flake.packages
            // {
              default = flake.packages.${defaultPackageName};
            };

          apps =
            flake.apps
            // {
              update-all-materialized = {
                type = "app";

                program =
                  (pkgs.writeShellScript "update-all-materialized" ''
                    set -eEuo pipefail
                    cd "$(${pkgs.git}/bin/git rev-parse --show-toplevel)"
                    ${pkgs.lib.concatStringsSep "\n" (map (system: ''
                        echo "Updating materialization for ${system}" >&2
                        ${packagesBySystem.${system}.update-all-materialized}
                      '')
                      supportedSystems)}
                  '')
                  .outPath;
              };
            };
          hydraJobs = builtins.removeAttrs self.packages.${evalSystem} ["default"];
        });
  in
    flake inputs
    // {
      hydraJobs = {
        nixpkgs ? inputs.nixpkgs,
        flake-utils ? inputs.flake-utils,
        haskell-nix ? inputs.haskell-nix,
      } @ overrides: let
        flake' = flake (inputs // overrides // {self = flake';});
        evalSystem = "x86_64-linux";
        pkgs = nixpkgs.legacyPackages.${evalSystem};
      in
        flake'.hydraJobs
        // {
          forceNewEval = pkgs.writeText "forceNewEval" (self.rev or self.lastModified);
          required = pkgs.releaseTools.aggregate {
            name = "cicero-pipe";
            constituents =
              builtins.concatMap (
                system:
                  map (x: "${x}.${system}") (builtins.attrNames flake'.hydraJobs)
              )
              supportedSystems;
          };
        };
    };
}
