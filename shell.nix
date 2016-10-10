let 
  lts = "lts-6_7";
in 
  with import <nixpkgs> { };
  haskell.lib.buildStackProject {
     ghc = haskell.packages.${lts}.ghc;
     name = "rscoin";
     buildInputs = [ zlib glib cairo gnome2.pango gnome3.gtk git cabal-install
                     openssh autoreconfHook stack nodejs haskellPackages.purescript ];
   }
