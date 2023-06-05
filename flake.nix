{
  description = "Try-phi front end";

  inputs = {
    nixpkgs_.url = "github:deemp/flakes?dir=source-flake/nixpkgs";
    nixpkgs.follows = "nixpkgs_/nixpkgs";
    flake-utils_.url = "github:deemp/flakes?dir=source-flake/flake-utils";
    flake-utils.follows = "flake-utils_/flake-utils";
    drv-tools.url = "github:deemp/flakes?dir=drv-tools";
    purescript-tools.url = "github:deemp/flakes?dir=language-tools/purescript";
    devshell.url = "github:deemp/flakes?dir=devshell";
    codium.url = "github:deemp/flakes?dir=codium";
    haskell-tools.url = "github:deemp/flakes?dir=language-tools/haskell";
    nixpkgs-purescript.url = "github:deemp/nixpkgs";
    purescript = {
      url = "github:purescript/purescript";
      flake = false;
    };
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = inputs.nixpkgs.legacyPackages.${system};
        pkgs-purescript = inputs.nixpkgs-purescript.legacyPackages.${system};
        shellTools = inputs.purescript-tools.shellTools.${system};
        inherit (inputs.devshell.functions.${system}) mkShell mkCommands mkRunCommands;
        inherit (inputs.drv-tools.functions.${system}) mkShellApps mkBin;
        inherit (inputs.codium.configs.${system}) extensions settingsNix;
        inherit (inputs.codium.functions.${system}) writeSettingsJSON mkCodium;
        inherit (inputs.haskell-tools.functions.${system}) toolsGHC;
        inherit (builtins) attrValues;

        scripts = mkShellApps {
          default = {
            text = "${pkgs.nodejs_18}/bin/npm run quick-start";
            description = "Run front";
          };
          buildGHPages = {
            text = ''${pkgs.nodejs_18}/bin/npm run build:gh-pages'';
            description = "Build GitHub Pages";
          };
        };

        tools = [
          pkgs-purescript.purescript
          pkgs-purescript.nodePackages.purs-tidy
          pkgs.nodejs_18
          pkgs.spago
          pkgs.dhall-lsp-server
          pkgs.nodePackages.purescript-language-server
        ];

        packages = {
          codium = mkCodium {
            extensions = { inherit (extensions) nix misc github markdown purescript; };
            runtimeDependencies = tools;
          };
          writeSettings = writeSettingsJSON settingsNix;
        } // scripts;

        devShells.default = mkShell {
          packages = tools;
          commands =
            mkCommands "tools" tools ++
            mkRunCommands "ide" { "codium ." = packages.codium; inherit (packages) writeSettings; } ++
            mkRunCommands "scripts" { inherit (packages) default buildGHPages; }
          ;
        };
      in
      {
        inherit packages devShells;
      });

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
