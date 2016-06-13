{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-6_1" }:

let

  inherit (nixpkgs) pkgs;
  dependencies = 
    with nixpkgs; 
    [zlib zlib.out git openssh autoreconfHook pkgconfig glib cairo pango gtk3];
  f = { mkDerivation, stdenv, base, zlib }:
      mkDerivation {
        pname = "rscoin";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = dependencies;
        executableHaskellDepends = dependencies;
        testHaskellDepends = dependencies;
        libraryPkgconfigDepends = dependencies;
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f { };

in

  if pkgs.lib.inNixShell then drv.env else drv
