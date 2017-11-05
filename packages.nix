let
  pkgs = import ./pkgs.nix { config = { allowUnfree = true; }; };
  # Utilities to modify haskell packages
  justStaticExecutables = pkgs.haskell.lib.justStaticExecutables;
  dontHaddock = pkgs.haskell.lib.dontHaddock;
  dontCheck = pkgs.haskell.lib.dontCheck;
  # Filter to exclude garbage from sources of derivations
  filterHaskell = src:
    let f = name: type:
      let base = builtins.baseNameOf name;
      in pkgs.lib.cleanSourceFilter name type &&
        (type != "directory" || base != "dist");
    in builtins.filterSource f src;
  addSrcFilter = drv: pkgs.haskell.lib.overrideCabal drv (drv: {
      src = filterHaskell drv.src;
    });

  # Package set for project, possibly overrides default nix set
  packages = pkgs.haskellPackages.override {
    overrides = haskellPackagesNew: haskellPackagesOld:
      let
        call = haskellPackagesNew.callPackage;
        cabalCall  = name: path: addSrcFilter (haskellPackagesNew.callCabal2nix name path { });
        cabalCallE = name: path: addSrcFilter (justStaticExecutables (haskellPackagesNew.callCabal2nix name path { }));
      in rec {
        servant-auth-token-acid = call ./nixdeps/servant-auth-token-acid.nix {};
        immortelle-cms-api     = cabalCall  "immortelle-cms-api" ./immortelle-cms-api;
        immortelle-cms-server  = cabalCallE "immortelle-cms-server" ./immortelle-cms-server;
      };
    };
in packages
