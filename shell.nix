let 
  lts = "lts-6_7";
in 
  with import <nixpkgs> { };
  haskell.lib.buildStackProject {
     ghc = haskell.packages.${lts}.ghc;
     name = "rscoin";
     buildInputs = [ zlib git cabal-install
                     openssh autoreconfHook stack nodejs haskellPackages.purescript ];
   }
