{ 
  description = "A flake enabling tooling for clash-formal-playground";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    clash-compiler.url = "github:clash-lang/clash-compiler";
    clash-protocols = {
      # Change this to be the official repository as soon as it gets merged
      url = "github:jaschutte/flakey-clash-protocols";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.clash-compiler.follows = "clash-compiler";
    };
    ecpprog.url = "github:diegodiv/ecpprog";
  };
  outputs = { nixpkgs, ecpprog, flake-utils, clash-compiler, clash-protocols, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # Sources for things which do not yet have a flake
        non-flake-srcs = {
          clash-cores = pkgs.fetchFromGitHub {
            owner = "clash-lang";
            repo = "clash-cores";
            rev = "34fb54f80a89205640cdf1fdab678139493e097e";
            hash = "sha256-ZDxLDBLqlZ4OCORPb7pnT3Ini5LoZlKN58HoWFgeeWw=";
          };
        };

        # Patch programs to be the correct version we want
        overlay = final: prev: {
          clash-prelude = clash-compiler.packages.${system}.clash-prelude;
          clash-prelude-hedgehog = clash-compiler.packages.${system}.clash-prelude-hedgehog;
          clash-lib = clash-compiler.packages.${system}.clash-lib;
          clash-ghc = clash-compiler.packages.${system}.clash-ghc;

          circuit-notation = clash-protocols.packages.${system}.circuit-notation;
          clash-protocols-base = clash-protocols.packages.${system}.clash-protocols-base;
          clash-protocols = clash-protocols.packages.${system}.clash-protocols;

          string-interpolate = clash-input-pkgs.haskell.lib.doJailbreak (prev.string-interpolate);

          clash-cores = final.developPackage {
            root = non-flake-srcs.clash-cores.outPath;
            overrides = overlay;
          };
        };
        clash-input-pkgs = clash-compiler.inputs.nixpkgs.legacyPackages.${system};
        hs-pkgs = clash-input-pkgs.haskell.packages.ghc910.extend overlay;

        # Packages built by this repository
        clash-ila = hs-pkgs.developPackage {
          root = ./ila;
          overrides = overlay;
        };
        ila-cli = import ./ila-cli/Cargo.nix {
          nixpkgs = nixpkgs;
          pkgs = pkgs;
        };

        # General packages from nixpkgs
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        devShells.default = hs-pkgs.shellFor {
          packages = p: [
            clash-ila
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

              # Surfer to view VCD
              pkgs.surfer

              # Program the FPGA
              ecpprog.defaultPackage.${system}
            ]
          ;
        };
        packages = {
          clash-ila = clash-ila;
          ila-cli = ila-cli;

          default = clash-ila;
        };
        apps.default = {
          type = "app";
          program = "${ila-cli.rootCrate.build}/bin/ila-cli";
        };
      });
}
