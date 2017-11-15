{
  network.description = "Immortelle CMS system";
  network.enableRollback = true;

  immortelleServer = { config, ... }: {
    imports = [
      ../immortelle-cms-server/service.nix
    ];
    services.immortelle-cms-server.enable = true;
    networking = {
      firewall = {
        enable = true;
        allowedTCPPorts = [ 80 443 ];
      };
    };
  };
}
