{ 
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
        non-flake-srcs = {
          clash-protocols = pkgs.fetchFromGitHub {
            owner = "clash-lang";
            repo = "clash-protocols";
            rev = "2ef336c2a0ca969cc5c8ad9cdb54cca5bd47f0bf";
            hash = "sha256-sJX78AELxzV+y8GvqcLGvE1fWreWZMkhL18fdxsomjA=";
          };
          clash-cores = pkgs.fetchFromGitHub {
            owner = "clash-lang";
            repo = "clash-cores";
            rev = "34fb54f80a89205640cdf1fdab678139493e097e";
            hash = "sha256-ZDxLDBLqlZ4OCORPb7pnT3Ini5LoZlKN58HoWFgeeWw=";
          };
          circuit-notation = pkgs.fetchFromGitHub {
            owner = "cchalmers";
            repo = "circuit-notation";
            rev = "564769c52aa05b90f81bbc898b7af7087d96613d";
            hash = "sha256-sPfLRjuMxqVRMzXrHRCuKKrdTdqgAJ33pf11DoTP84Q=";
          };
        };
        overlay = final: prev: {
          cabal-install = nixpkgs.legacyPackages.${system}.cabal-install;
          clash-prelude = clash-compiler.packages.${system}.clash-prelude;
          clash-prelude-hedgehog = clash-compiler.packages.${system}.clash-prelude-hedgehog;
          clash-lib = clash-compiler.packages.${system}.clash-lib;
          clash-ghc = clash-compiler.packages.${system}.clash-ghc;

          string-interpolate = clash-input-pkgs.haskell.lib.doJailbreak (prev.string-interpolate);

          circuit-notation = final.developPackage {
            root = non-flake-srcs.circuit-notation.outPath;
            overrides = overlay;
          };
          clash-protocols-base = final.developPackage {
            root = "${non-flake-srcs.clash-protocols.outPath}/clash-protocols-base";
            overrides = overlay;
          };
          clash-protocols = final.developPackage {
            root = "${non-flake-srcs.clash-protocols.outPath}/clash-protocols";
            overrides = overlay;
          };
          clash-cores = final.developPackage {
            root = non-flake-srcs.clash-cores.outPath;
            overrides = overlay;
          };
          clash-ila = hs-pkgs.developPackage {
            root = ./ila;
            overrides = overlay;
          };
        };
        clash-input-pkgs = clash-compiler.inputs.nixpkgs.legacyPackages.${system};
        hs-pkgs = clash-input-pkgs.haskell.packages.ghc910.extend overlay;
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        hs = hs-pkgs;
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
              hs-pkgs.cabal-install
              hs-pkgs.haskell-language-server

              # Rust subproject
              pkgs.cargo
              pkgs.rustc
              pkgs.clippy
              # Required as we depend on libudev for tty iteration
              pkgs.pkg-config
              pkgs.udev

              # Surfer to view VCD
              pkgs.surfer

              # Program the FPGA
              ecpprog.defaultPackage.${system}
            ]
          ;
        };
        packages.default = hs-pkgs.clash-ila;
      });
}
