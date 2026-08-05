{
  description = "Reproducible PubMed/MEDLINE publication-delay pipeline";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs @ { flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem =
        { self'
        , pkgs
        , lib
        , ...
        }:
        let
          python = pkgs.python312;
          pythonPackages = python.pkgs;

          pySrc = lib.cleanSourceWith {
            src = ./.;
            filter = path: type:
              let
                base = baseNameOf path;
                rel = lib.removePrefix (toString ./. + "/") (toString path);
              in
              !(lib.hasInfix "/__pycache__/" ("/" + rel + "/"))
              && !(lib.hasSuffix ".pyc" base)
              && base != ".pytest_cache"
              && base != ".mypy_cache"
              && base != ".ruff_cache"
              && base != ".swival"
              && base != ".agents"
              && base != ".claude"
              && base != ".codex"
              && base != ".cursor"
              && base != ".windsurf"
              && base != ".git"
              && base != "result"
              && base != "swival.toml"
              && base != "AGENTS.md"
              && base != "CLAUDE.md"
              && base != "GEMINI.md"
              && base != "uv.lock"
              && base != ".mcp.json"
              && !(lib.hasPrefix "prompts/" rel)
              && rel != "scripts/swival-review.sh"
              && !(lib.hasPrefix "data/raw_data/" rel)
              && !(lib.hasPrefix "data/temp_data/" rel)
              && !(lib.hasPrefix "data/processed_data/" rel)
              && !(lib.hasPrefix "data/manifests/" rel)
              && !(lib.hasPrefix "data/external/" rel);
          };

          pubdelays = pythonPackages.buildPythonApplication {
            pname = "pubdelays";
            version = "0.1.0";
            src = pySrc;
            pyproject = true;

            build-system = with pythonPackages; [
              setuptools
              wheel
            ];

            dependencies = with pythonPackages; [
              lxml
              polars
            ];

            nativeCheckInputs = with pythonPackages; [
              pytest
            ];

            checkPhase = ''
              runHook preCheck
              pytest -q
              runHook postCheck
            '';

            pythonImportsCheck = [
              "pubdelays"
              "pubdelays.cli"
              "pubdelays.aggregate"
              "pubdelays.manifest"
              "pubdelays.parser.medline"
              "pubdelays.transform.articles"
              "pubdelays.external.scimago"
              "pubdelays.external.wos"
              "pubdelays.external.doaj"
              "pubdelays.external.npi"
              "pubdelays.external.retraction_watch"
            ];
          };

          pythonEnv = python.withPackages (ps:
            with ps; [
              lxml
              pytest
              polars
            ]);

          modelPythonEnv = python.withPackages (ps:
            with ps; [
              catboost
              lxml
              matplotlib
              numpy
              pandas
              polars
              pytest
              scikit-learn
            ]);

          rEnv = pkgs.rWrapper.override {
            packages = with pkgs.rPackages; [
              dplyr
              glue
              jsonlite
              fuzzyjoin
              tidyr
              readr
              lubridate
              stringr
              tibble
              purrr
            ];
          };
        in
        {
          packages.default = pubdelays;
          packages.pubdelays = pubdelays;

          apps.default = {
            type = "app";
            program = "${self'.packages.pubdelays}/bin/pubdelays-pipeline";
          };
          apps.pubdelays-pipeline = self'.apps.default;

          checks.default = pubdelays;

          devShells.default = pkgs.mkShell {
            packages = [
              pythonEnv
              rEnv
              pkgs.uv
              pkgs.shellcheck
              pkgs.ruff
              pkgs.nixpkgs-fmt
            ];

            PYTHONPATH = "${toString ./.}/src";
            shellHook = ''
              echo "pubdelays dev shell: run pubdelays-pipeline --help"
            '';
          };

          devShells.model = pkgs.mkShell {
            packages = [ modelPythonEnv ];
            PYTHONPATH = "${toString ./.}/src:${toString ./.}";
            shellHook = ''
              echo "pubdelays model shell: run python -m pubdelays_analysis.boosting --help"
            '';
          };

          formatter = pkgs.nixpkgs-fmt;
        };
    };
}
