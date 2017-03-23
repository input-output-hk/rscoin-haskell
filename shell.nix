let 
  lts = "ghc7103";
in 
  with import <nixpkgs> { };
  haskell.lib.buildStackProject {
     ghc = haskell.packages.${lts}.ghc;
     name = "rscoin";
     buildInputs = [ zlib glib cairo gnome2.pango gnome3.gtk git cabal-install
                     openssh autoreconfHook stack nodejs gmp ];
   }
