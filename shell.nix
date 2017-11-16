{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, cassava, mwc-random
      , optparse-applicative, pipes, pipes-bytestring, pipes-concurrency
      , pipes-csv, primitive, split, stdenv, transformers, vector
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
        description = "Reimplementation of R. Bertram works.";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
