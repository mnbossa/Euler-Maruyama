let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {

          mkDerivation = args: haskellPackagesOld.mkDerivation (args // {
            #enableLibraryProfiling = true;
          });

          pituitary =
            haskellPackagesNew.callPackage ./default.nix {
            };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  rec {
    pituitary = pkgs.haskell.lib.doBenchmark pkgs.haskellPackages.pituitary;
}
