let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.callPackage ./backend.nix { }
