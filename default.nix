{ mkDerivation, async, base, bytestring, cassava, criterion
, deepseq, mwc-random, optparse-applicative, pipes
, pipes-bytestring, pipes-concurrency, pipes-csv, primitive, split
, stdenv, transformers, vector
}:
mkDerivation {
  pname = "pituitary";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring cassava mwc-random optparse-applicative pipes
    pipes-bytestring pipes-concurrency pipes-csv primitive split
    transformers vector
  ];
  executableHaskellDepends = [
    async base bytestring cassava mwc-random optparse-applicative pipes
    pipes-bytestring pipes-concurrency pipes-csv primitive split
    transformers vector
  ];
  benchmarkHaskellDepends = [
    async base bytestring cassava criterion deepseq mwc-random
    optparse-applicative pipes pipes-bytestring pipes-concurrency
    pipes-csv primitive split transformers vector
  ];
  description = "Reimplementation of R. Bertram works.";
  license = stdenv.lib.licenses.mit;
}
