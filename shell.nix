{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-6_7" }:

let
  inherit (nixpkgs) pkgs;
  f = { mkDerivation, stdenv, pkgconfig }:
      mkDerivation {
        pname = "rscoin";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryPkgconfigDepends = 
            with nixpkgs; [zlib glib cairo gnome.pango gnome3.gtk git 
                           openssh autoreconfHook stack nodejs haskellPackages.purescript ];
        license = stdenv.lib.licenses.gpl3;
      };
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  drv = haskellPackages.callPackage f { };
in
  if pkgs.lib.inNixShell then drv.env else drv
