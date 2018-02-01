{ compiler ? "ghc822" }:
let
  nixpkgs = import <nixpkgs> { };
in
  (import ./default.nix { inherit nixpkgs compiler; }).env
