{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, cassava, cmdargs
      , mwc-random, primitive, split, stdenv, transformers
      }:
      mkDerivation {
        pname = "pituitary";
        version = "0.0.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring cassava cmdargs mwc-random primitive split
          transformers
        ];
        executableHaskellDepends = [
          base bytestring cassava cmdargs mwc-random primitive split
          transformers
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
