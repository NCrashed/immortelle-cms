let
  reflex-platform = import ../reflex-platform {};
  pkgs = reflex-platform.nixpkgs;

  # Utilities to modify haskell packages
  dontHaddock = pkgs.haskell.lib.dontHaddock;
  dontCheck = pkgs.haskell.lib.dontCheck;
  justBrowserOutput = drv: pkgs.haskell.lib.overrideCabal drv (drv: {
    postFixup = ''
      rm -rf $out/lib $out/nix-support $out/share/doc
      cd $out/bin
      find . -maxdepth 1 -type f ! -name '*.jsexe' -delete
      cd ../..
      '';
  });
  optimizeCC = drv: pkgs.haskell.lib.overrideCabal drv (drv: {
    postFixup = ''
      cd $out/bin/immortelle-cms-frontend.jsexe
      #cp all.js all.min.js
      ${pkgs.closurecompiler}/bin/closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS \
        --externs=all.js.externs \
        --externs=${../immortelle-cms-server/static/jquery-1.9.1.js} \
        --externs=${../immortelle-cms-server/static/jquery-1.9.js.externs} \
        --externs=${../immortelle-cms-server/static/jquery.scrollTo.min.js} \
        --externs=${../immortelle-cms-server/static/moment-with-locales.js} \
        --externs=${../immortelle-cms-server/static/bootstrap/js/bootstrap.min.js} \
        --externs=${../immortelle-cms-server/static/bootstrap/js/ripples.min.js} \
        --externs=${../immortelle-cms-server/static/bootstrap/js/material.min.js} \
        --externs=${../immortelle-cms-server/static/bootstrap-material-datetimepicker.js} \
        --externs=${../immortelle-cms-server/static/runmain.js} \
        --jscomp_off=duplicate \
        --jscomp_off=undefinedVars \
        --jscomp_off=externsValidation \
        > all.min.js
    '';
  });

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

  # Extend given packages set
  packages = reflex-platform.ghcjs.extend (self: super:
    let
      cabalCall  = name: path: dontHaddock (dontCheck (addSrcFilter (self.callCabal2nix name path { })));
      cabalCallE = name: path: dontHaddock (dontCheck (addSrcFilter (justBrowserOutput (self.callCabal2nix name path { }))));
    in rec {
      swagger2 = self.callPackage ../nixdeps/swagger2.nix {};
      servant-auth = self.callPackage ../nixdeps/servant-auth.nix {};
      reflex-material-bootstrap = self.callPackage ../nixdeps/reflex-material-bootstrap.nix {};
      servant-reflex = dontHaddock (dontCheck (self.callPackage ../nixdeps/servant-reflex.nix { }));
      immortelle-cms-api = dontHaddock (dontCheck (self.callCabal2nix "immortelle-cms-api" ../immortelle-cms-api {}));
      immortelle-cms-frontend = optimizeCC (dontHaddock (dontCheck (self.callCabal2nix "immortelle-cms-frontend" ./. {})));
    }
  );
in packages
