{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.matrix-conduit;
  well_known_server = pkgs.writeText "well-known-matrix-server" ''
    {
      "m.server": "matrix.${cfg.settings.global.server_name}:443"
    }
  '';
  well_known_client = pkgs.writeText "well-known-matrix-client" ''
    {
      "m.homeserver": {
        "base_url": "https://matrix.${cfg.settings.global.server_name}"
      }
    }
  '';
in
  with lib; {
    options.services.my-matrix = {
      enable =
        mkEnableOption
        "enable the matrix service";

      server_name = mkOption {
        type = types.str;
        default = "example.com";
        description = ''
          The server name for the matrix server
        '';
      };
    };

    config = mkIf config.services.my-matrix.enable {
      environment.persistence."/keep".directories = [
        "/var/lib/private/matrix-conduit"
      ];
      services.matrix-conduit.enable = true;

      systemd.services.conduit.serviceConfig.ExecStart = lib.mkForce "${cfg.package}/bin/conduwuit";

      services.matrix-conduit.settings = {
        global = {
          inherit (config.services.my-matrix) server_name;
          allow_federation = true;
          allow_encryption = true;
          allow_registration = true;
          database_backup_path = "/var/lib/matrix-conduit/backups";
          registration_token_file = config.age.secrets.conduwuit-registration-token.path;
          max_request_size = 20000000;
          max_concurrent_requests = 100;
          turn_uris = ["turn:staticauth.turn.openrelay.metered.ca:443?transport=udp" "turn:staticauth.turn.openrelay.metered.ca:443?transport=tcp"];
          turn_secret = "openrelayprojectsecret";
          port = 6167;
          database_backend = "rocksdb";
        };
      };
      services.matrix-conduit.package = pkgs.conduwuit-latest;
      services.nginx.virtualHosts = {
        "matrix.${cfg.settings.global.server_name}" = {
          locations."/_matrix" = {
            proxyPass = "http://backend_conduit$request_uri";
            proxyWebsockets = true;
            extraConfig = ''
              proxy_set_header Host $host;
              proxy_buffering off;
            '';
          };
          extraConfig = ''
            merge_slashes off;
          '';
        };
        "${cfg.settings.global.server_name}" = {
          locations."=/.well-known/matrix/server" = {
            alias = "${well_known_server}";

            extraConfig = ''
              default_type application/json;
            '';
          };

          locations."=/.well-known/matrix/client" = {
            alias = "${well_known_client}";

            extraConfig = ''
              default_type application/json;
              add_header Access-Control-Allow-Origin "*";
            '';
          };
        };
      };
      services.nginx.upstreams = {
        "backend_conduit" = {
          servers = {
            "[::1]:${toString cfg.settings.global.port}" = {};
          };
        };
      };
    };
  }
