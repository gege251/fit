# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "fit";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc924";
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.${compiler};


        packageName = "fit";
      in
      {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self {
            # Dependency overrides go here
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = [
            haskellPackages.ghc
            haskellPackages.haskell-language-server # you must build it with your ghc to work
            haskellPackages.cabal-install
            haskellPackages.hlint
            haskellPackages.fourmolu
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };

        checks = self.packages.${system}.${packageName};
      });
}
