{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./haskell-src-edit.nix {
  inherit (nixpkgs.stdenv.lib) sourceByRegex;
}
