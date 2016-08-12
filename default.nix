{ mkDerivation, acid-state, async, base, base64-bytestring, binary
, blake2, bytestring, cereal, clock, conduit-extra, containers
, data-default, directory, ed25519, either, exceptions, file-embed
, filepath, formatting, hashable, hslogger, hspec, lens
, monad-control, monad-loops, MonadRandom, msgpack, msgpack-rpc
, mtl, optparse-applicative, optparse-generic, pqueue, QuickCheck
, random, safe, safecopy, serokell-core, stdenv, stm, temporary
, text, text-format, time, time-units, transformers
, transformers-base, tuple, unordered-containers, vector
, makeWrapper 
}:
mkDerivation {
  pname = "rscoin";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  libraryHaskellDepends = [
    acid-state base base64-bytestring binary blake2 bytestring cereal
    conduit-extra containers data-default directory ed25519 either
    exceptions file-embed filepath hashable hslogger lens
    monad-control monad-loops MonadRandom msgpack msgpack-rpc mtl
    pqueue QuickCheck random safe safecopy serokell-core stm text
    text-format time time-units transformers transformers-base tuple
    unordered-containers vector
  ];
  executableHaskellDepends = [
    acid-state async base clock containers exceptions filepath
    formatting lens mtl optparse-applicative optparse-generic
    safecopy serokell-core stm temporary text text-format time-units
    transformers tuple
  ];
  testHaskellDepends = [
    acid-state async base bytestring containers data-default exceptions
    hspec lens MonadRandom msgpack msgpack-rpc mtl QuickCheck random
    safe safecopy serokell-core stm text time-units transformers tuple
    vector
  ];
  buildDepends = [ makeWrapper ];
  license = stdenv.lib.licenses.gpl3;
}
