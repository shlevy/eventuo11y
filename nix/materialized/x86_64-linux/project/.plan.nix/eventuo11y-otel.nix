{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "3.0";
      identifier = { name = "eventuo11y-otel"; version = "0.1.0.2"; };
      license = "Apache-2.0";
      copyright = "Copyright 2023 Shea Levy.";
      maintainer = "shea@shealevy.com";
      author = "Shea Levy";
      homepage = "";
      url = "";
      synopsis = "OpenTelemetry-based rendering for eventuo11y";
      description = "Render [eventuo11y](https://hackage.haskell.org/package/eventuo11y) 'Observe.Event.Event's\nto OpenTelemetry traces.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "CHANGELOG.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."eventuo11y" or (errorHandler.buildDepError "eventuo11y"))
          (hsPkgs."hs-opentelemetry-api" or (errorHandler.buildDepError "hs-opentelemetry-api"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        modules = [ "Observe/Event/Render/OpenTelemetry" ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../eventuo11y-otel; }