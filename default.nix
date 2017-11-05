let
  pkgs = import ./pkgs.nix { inherit config; };
  projectPackages = import ./packages.nix;

  # Configure project packages
  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskellPackages = projectPackages;
    };
  };

# Merge all packages into single derivative to place in single result symlink
in with pkgs.haskellPackages; pkgs.buildEnv {
  name = "immortelle-cms";
  paths = [
    immortelle-cms-server
  ];
}
