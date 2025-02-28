
{ 
  nixConfig.extra-substituters = [ "http://warnsveld" ];
  nixConfig.extra-trusted-public-keys = [ "warnsveld:kt+WouF9QqWCs9tmRt+05V21V575togaM8/MFtaKeCg=" ];

  description = "A flake enabling tooling for clash-formal-playground";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    clash-compiler.url = "github:clash-lang/clash-compiler/dd/nix-update";
    ecpprog.url = "github:diegodiv/ecpprog";
  };
  outputs = { self, nixpkgs, ecpprog, flake-utils, clash-compiler, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = clash-compiler.inputs.nixpkgs.legacyPackages.${system};
          overlay = final: prev: {
            clash-cores = clash-compiler.packages.${system}.clash-cores;
            clash-prelude = clash-compiler.packages.${system}.clash-prelude;
            clash-lib = clash-compiler.packages.${system}.clash-lib;
            clash-ghc = clash-compiler.packages.${system}.clash-ghc;
            cabal-install = nixpkgs.legacyPackages.${system}.cabal-install;
          };
          # TODO: refer dynamically to the right ghc version (using clash-compiler's default).
          # Might require some changes on clash-compiler's flake.
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
            with pkgs; [
              # For interacting with the OrangeCrab board.
              yosys trellis nextpnr
              gnumake
              # To have some nice HTML to publish for Coq proofs.
              python311Packages.alectryon

              haskell-language-server
            ] ++
            (with pkgs.coqPackages; [
              coq mathcomp hierarchy-builder mathcomp-word serapi
            ])
            ++
            (with myHsPkgs; [ cabal-install haskell-language-server ])
            ++
            [ ecpprog.defaultPackage.${system} ]
          ;
        };
      });
}
