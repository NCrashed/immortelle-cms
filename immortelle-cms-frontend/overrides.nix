{ reflex-platform, ... }:
let
  config = { allowUnfree = true; };
  pkgs = import ../pkgs.nix { inherit config; };
  dontCheck = pkgs.haskell.lib.dontCheck;
  dontHaddock = pkgs.haskell.lib.dontHaddock;
in
reflex-platform.ghcjs.override {
  overrides = self: super: {
    servant-auth = self.callPackage ../../nixdeps/servant-auth.nix {};
    reflex-material-bootstrap = self.callPackage ../../nixdeps/reflex-material-bootstrap.nix {};
    immortelle-cms-api = dontHaddock (dontCheck (self.callCabal2nix "immortelle-cms-api" ../immortelle-cms-api {}));
    servant-reflex = dontHaddock (dontCheck (self.callPackage ../../nixdeps/servant-reflex.nix { }));
  };
}
