
{ 
  nixConfig.extra-substituters = [ "http://warnsveld" ];
  nixConfig.extra-trusted-public-keys = [ "warnsveld:kt+WouF9QqWCs9tmRt+05V21V575togaM8/MFtaKeCg=" ];

  description = "A flake enabling tooling for clash-formal-playground";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    clash-compiler.url = "github:clash-lang/clash-compiler";
    ecpprog.url = "github:diegodiv/ecpprog";
  };
  outputs = { nixpkgs, ecpprog, flake-utils, clash-compiler, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = final: prev: {
          clash-cores = clash-compiler.packages.${system}.clash-cores;
          clash-prelude = clash-compiler.packages.${system}.clash-prelude;
          clash-lib = clash-compiler.packages.${system}.clash-lib;
          clash-ghc = clash-compiler.packages.${system}.clash-ghc;
          cabal-install = nixpkgs.legacyPackages.${system}.cabal-install;
        };
        pkgs = clash-compiler.inputs.nixpkgs.legacyPackages.${system};
        myHsPkgs = (pkgs.haskell.packages.ghc910.extend overlay).extend
          (pkgs.haskell.lib.compose.packageSourceOverrides {
            orangecrabby = ./orangecrab;
          });
      in
      {
        devShells.default = myHsPkgs.shellFor {
          packages = p: [ p.orangecrabby ];
          inputsFrom = [
            clash-compiler.packages.${system}.clash-lib.env
            clash-compiler.packages.${system}.clash-ghc.env
          ];
          nativeBuildInputs = 
            [
              # For interacting with the OrangeCrab board.
              pkgs.yosys
              pkgs.trellis
              pkgs.nextpnr
              pkgs.gnumake

              # Haskell stuff
              myHsPkgs.cabal-install
              myHsPkgs.haskell-language-server
              myHsPkgs.clash-ghc

              # Idk
              ecpprog.defaultPackage.${system}
            ]
          ;
        };
      });
}
