
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
        clashPkgs = clash-compiler.inputs.nixpkgs.legacyPackages.${system};
        hsPkgs = clashPkgs.haskell.packages.ghc910.extend (overlay);
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = pkgs.mkShell {
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
              hsPkgs.cabal-install
              hsPkgs.haskell-language-server

              # Rust subproject
              pkgs.cargo
              pkgs.rustc
              pkgs.clippy
              # Required as we depend on libudev for tty iteration
              pkgs.pkg-config
              pkgs.udev

              # Surfer to view VCD
              pkgs.surfer

              # Idk
              ecpprog.defaultPackage.${system}
            ]
          ;
        };
      });
}
