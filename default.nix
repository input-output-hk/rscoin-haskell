{ mkDerivation, acid-state, aeson, async, base, base64-bytestring
, binary, bytestring, cereal, conduit-extra, containers, cryptohash
, data-default, directory, ed25519, either, exceptions, file-embed, temporary
, filepath, hashable, hslogger, hspec, lens, monad-control, formatting
, monad-loops, MonadRandom, msgpack, msgpack-aeson, msgpack-rpc
, mtl, optparse-applicative, pqueue, QuickCheck, random, safe
, safecopy, serokell-core, stdenv, stm, text, text-format, time
, time-units, transformers, transformers-base, tuple
, unordered-containers, vector
}:
mkDerivation {
  pname = "rscoin";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  doCheck = false;
  libraryHaskellDepends = [
    acid-state aeson base base64-bytestring binary bytestring cereal
    conduit-extra containers cryptohash data-default directory ed25519
    either exceptions file-embed filepath hashable hslogger lens
    monad-control monad-loops MonadRandom msgpack msgpack-aeson
    msgpack-rpc mtl pqueue QuickCheck random safe safecopy
    serokell-core stm text text-format time time-units transformers
    transformers-base tuple unordered-containers vector
  ];
  executableHaskellDepends = [
    acid-state aeson base base64-bytestring binary bytestring cereal
    conduit-extra containers cryptohash data-default directory ed25519
    exceptions filepath hashable hslogger hspec lens monad-control
    monad-loops MonadRandom msgpack msgpack-aeson msgpack-rpc mtl
    optparse-applicative pqueue QuickCheck random safe safecopy
    serokell-core stm text text-format time time-units transformers
    transformers-base tuple unordered-containers vector
  ];
  testHaskellDepends = [
    acid-state async base bytestring containers data-default exceptions
    hspec lens MonadRandom msgpack msgpack-rpc mtl QuickCheck random
    safe safecopy serokell-core stm text time-units transformers tuple
    vector
  ];
  license = stdenv.lib.licenses.gpl3;
}
