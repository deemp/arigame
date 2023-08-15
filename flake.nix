{
  inputs.flakes.url = "github:deemp/flakes";
  outputs = inputs:
    let flakes = inputs.flakes; in
    flakes.makeFlake {
      inputs = {
        inherit (inputs.flakes.all)
          nixpkgs lima formatter codium drv-tools devshell
          flakes-tools workflows purescript-tools;
        inherit flakes;
      };
      perSystem = { inputs, system }:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          purescript-tools = inputs.purescript-tools.packages.${system};
          inherit (inputs.devshell.lib.${system}) mkShell mkCommands mkRunCommands mkDefaultCommands;
          inherit (inputs.drv-tools.lib.${system}) mkShellApps mkBin;
          inherit (inputs.codium.lib.${system}) extensions extensionsCommon settingsNix settingsCommonNix writeSettingsJSON mkCodium;
          inherit (inputs.flakes-tools.lib.${system}) mkFlakesTools;
          inherit (inputs) workflows;

          tools = __attrValues {
            inherit (purescript-tools)
              purescript purs-tidy purescript-language-server
              nodejs_18 spago dhall-lsp-server;
          };

          packages = mkShellApps {
            default = {
              text = "${pkgs.nodejs_18}/bin/npm run quick-start";
              description = "Run dev";
              runtimeInputs = [ pkgs.nodejs_18 pkgs.spago purescript-tools.purescript ];
            };
            npmCleanCache = {
              text = ''npm cache clean --force'';
              runtimeInputs = [ pkgs.nodejs_18 ];
            };
            buildGHPages = {
              text = ''npm run build:gh-pages'';
              runtimeInputs = [ pkgs.nodejs_18 pkgs.spago purescript-tools.purescript ];
              description = "Build GitHub Pages";
            };
            codium = mkCodium {
              extensions = extensionsCommon // { inherit (extensions) purescript; };
              runtimeDependencies = tools;
            };
            writeSettings = writeSettingsJSON (settingsCommonNix // { inherit (settingsNix) vscode-dhall-lsp-server ide-purescript; });
            writeWorkflows = import ./nix-files/workflow.nix {
              name = "ci";
              inherit workflows system packages;
            };
            inherit (mkFlakesTools { dirs = [ "." ]; root = ./.; }) updateLocks saveFlakes format;
          };

          devShells.default = mkShell {
            packages = tools;
            commands =
              mkCommands "tools" tools
              ++ mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; }
              ++ mkRunCommands "scripts" { inherit (packages) default buildGHPages; }
              ++ mkRunCommands "infra" { inherit (packages) writeWorkflows; }
              ++ mkDefaultCommands "scripts"
                [
                  {
                    name = "npm run sass";
                    help = "sass watch .sass files and generate CSS";
                  }
                  {
                    name = "npm run dev";
                    help = "parcel watch files and reload browser window";
                  }
                  {
                    name = "npx lt -p <PORT> -s <SUBDOMAIN>";
                    help = "expose the app running at <PORT> at https://<SUBDOMAIN>.loca.lt";
                  }
                ]
            ;
          };
        in
        {
          inherit packages devShells;
          formatter = inputs.formatter.${system};
        };
    };

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://hydra.iohk.io"
      "https://deemp.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "deemp.cachix.org-1:9shDxyR2ANqEPQEEYDL/xIOnoPwxHot21L5fiZnFL18="
    ];
  };
}
