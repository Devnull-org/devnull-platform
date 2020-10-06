{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc884", withHoogle ? true }:
let
  inherit (nixpkgs) pkgs;
  pinnedUnstable =
    pkgs.fetchFromGitHub {
      owner = "NixOS";
      repo = "nixpkgs";
      rev = "6bb90a2d01448f581681a869433df347b001e25b";
      sha256 = "0375wv0w8ciddbji76k3anxzc0nqq09dmsyz6mdvbwj60dvhas7a";
    };
  unstable = import pinnedUnstable {};
  ghcVersion = unstable.haskell.packages.${compiler};
  hspkgs =
    if withHoogle
       then
         ghcVersion.override {
           overrides = (self: super: {
             ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
             ghcWithPackages = self.ghc.withPackages;
           });
         }
       else ghcVersion;

  origBuild = hspkgs.callPackage ./backend.nix {};
  drv = unstable.haskell.lib.overrideCabal origBuild (drv: {
    libraryToolDepends = [
      unstable.ghcid
      unstable.stdenv
      unstable.pkg-config
    ];
    librarySystemDepends = [ unstable.zlib ];
    license = unstable.stdenv.lib.licenses.bsd3;
    shellHook = ''
      '';
  });

in
  if pkgs.lib.inNixShell then drv.env else drv
