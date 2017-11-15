{ config, lib, ... }:
with lib;  # use the functions from lib, such as mkIf
let
  pkgs = import ../pkgs.nix { config = pkgconfig; };
  packages = import ../packages.nix;
  frontend = import ../immortelle-cms-frontend/frontend.nix;
  pkgconfig = {
    allowUnfree = true;
    packageOverrides = prkgs: rec {
      haskellPackages = packages;
    };
  };
  # the values of the options set for the service by the user of the service
  immortelle-cms-server-cfg = config.services.immortelle-cms-server;
in {
  ##### interface. here we define the options that users of our service can specify
  options = {
    # the options for our service will be located under services.immortelle-cms-server
    services.immortelle-cms-server = {
      enable = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether to enable Immortelle CMS server by default.
        '';
      };
      domain = mkOption {
        type = types.str;
        default = "immortelle.me";
        description = ''
          Domain of immortelle site.
        '';
      };
      config = mkOption {
        type = types.str;
        default = ''
          host: 127.0.0.1
          port: 3000
          static: ${./static}
          frontendBlob: ${frontend.immortelle-cms-frontend}/bin/immortelle-cms-frontend.jsexe/all.min.js
          detailedLogging: true
          state: /var/run/immortelle.db
          adminPassword: "${builtins.replaceStrings ["\n"] [""] (builtins.readFile ./password)}"
          cacheFolder: /var/run/immortelle.cache
        '';
        description = ''
          Configuration file for immortelle-cms-server.
        '';
      };
    };
  };

  ##### implementation
  config = mkIf immortelle-cms-server-cfg.enable { # only apply the following settings if enabled
    # Write configuration file to /etc/immortelle-cms-server.yaml
    environment.etc."immortelle-cms-server.yaml" = {
      text = immortelle-cms-server-cfg.config; # we can use values of options for this service here
    };
    # Create systemd service
    systemd.services.immortelle-cms-server = {
      enable = true;
      description = "Immortelle CMS server";
      after = [ "network.target" ];
      serviceConfig = {
          ExecStart = "${pkgs.haskellPackages.immortelle-cms-server}/bin/immortelle-cms /etc/immortelle-cms-server.yaml listen";
          Restart = "always";
          RestartSec = 30;
          User = "root";
        };
      wantedBy = ["multi-user.target"];
    };
    services.nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      virtualHosts."${immortelle-cms-server-cfg.domain}" = {
        enableACME = true;
        forceSSL = true;
        locations."/".proxyPass = "http://localhost:3000";
      };
    };
  };
}
