{config, ...}: let
  cfg = config.services.forgejo;
  srv = cfg.settings.server;
in {
  services.forgejo.enable = true;
  services.forgejo.settings = {
    server = {
      DOMAIN = "git.9000.dev";
      ROOT_URL = "https://${srv.DOMAIN}";
      HTTP_PORT = 3121;
    };
    service.DISABLE_REGISTRATION = true;
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
