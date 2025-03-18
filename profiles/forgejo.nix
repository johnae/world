{
  config,
  pkgs,
  ...
}: let
  cfg = config.services.forgejo;
  srv = cfg.settings.server;
in {
  services.forgejo.enable = false;
  services.forgejo.settings = {
    server = {
      DOMAIN = "git.9000.dev";
      ROOT_URL = "https://${srv.DOMAIN}";
      HTTP_PORT = 3121;
    };
    service.DISABLE_REGISTRATION = true;
  };
  systemd.services.tailscale-serve-ssh = {
    description = "Tailscale serve forgejo ssh";
    wantedBy = ["tailscaled.service"];
    after = ["tailscaled.service"];
    serviceConfig = {
      RestartSec = 10;
      Restart = "on-failure";
      ExecStart = pkgs.writeShellScript "tailscale-serve-ssh" ''
        ${pkgs.tailscale}/bin/tailscale serve --tcp 2222 tcp://localhost:22
      '';
    };
  };
  environment.persistence."/keep".directories = [
    cfg.stateDir
  ];
  services.restic.backups.remote.paths = [
    cfg.stateDir
  ];
  services.nginx.virtualHosts = {
    "${srv.DOMAIN}" = {
      locations."/" = {
        proxyPass = "http://localhost:3121";
      };
    };
  };
}
