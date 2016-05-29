{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, acid-state, aeson, async, base
      , base64-bytestring, binary, bytestring, cereal, conduit-extra
      , containers, cryptohash, data-default, directory, ed25519, either
      , exceptions, file-embed, filepath, hashable, hslogger, hspec, lens
      , monad-control, monad-loops, MonadRandom, msgpack, msgpack-aeson
      , msgpack-rpc, mtl, optparse-applicative, pqueue, QuickCheck
      , random, safe, safecopy, stdenv, stm, text
      , text-format, time, time-units, transformers, transformers-base
      , tuple, unordered-containers, vector
      , pkgconfig, cairo, gtk3
      }:
      mkDerivation {
        pname = "rscoin";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          acid-state aeson base base64-bytestring binary bytestring cereal
          conduit-extra containers cryptohash data-default directory ed25519
          either exceptions file-embed filepath hashable hslogger lens
          monad-control monad-loops MonadRandom msgpack msgpack-aeson
          msgpack-rpc mtl pqueue QuickCheck random safe safecopy
           stm text text-format time time-units transformers
          transformers-base tuple unordered-containers vector
        ];
        executableHaskellDepends = [
          acid-state aeson base base64-bytestring binary bytestring cereal
          conduit-extra containers cryptohash data-default directory ed25519
          exceptions filepath hashable hslogger hspec lens monad-control
          monad-loops MonadRandom msgpack msgpack-aeson msgpack-rpc mtl
          optparse-applicative pqueue QuickCheck random safe safecopy
           stm text text-format time time-units transformers
          transformers-base tuple unordered-containers vector
        ];
        testHaskellDepends = [
          acid-state async base bytestring containers data-default exceptions
          hspec lens MonadRandom msgpack msgpack-rpc mtl QuickCheck random
          safe safecopy  stm text time-units transformers tuple
          vector
        ];
        libraryPkgconfigDepends =
          (with nixpkgs; [zlib git openssh autoreconfHook]) ++
          [pkgconfig cairo gtk3];
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f { };

in

  if pkgs.lib.inNixShell then drv.env else drv
