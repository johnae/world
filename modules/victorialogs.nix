{
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  cfg = config.services.victorialogs;
  settingsFormat = pkgs.formats.yaml {};

  startCLIList =
    [
      "${cfg.package}/bin/victoria-logs"
      "-storageDataPath=/var/lib/${cfg.stateDir}"
      "-httpListenAddr=${cfg.listenAddress}"
    ]
    ++ lib.optionals (cfg.retentionPeriod != null) ["-retentionPeriod=${cfg.retentionPeriod}"]
    ++ cfg.extraOptions;
in {
  options.services.victorialogs = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Whether to enable VictoriaLogs in single-node mode.

        VictoriaLogs is a fast, cost-effective and scalable log search solution.
      '';
    };
    package = mkPackageOption pkgs "victorialogs" {};

    listenAddress = mkOption {
      default = ":9428";
      type = types.str;
      description = ''
        TCP address to listen for incoming http requests.
      '';
    };

    stateDir = mkOption {
      type = types.str;
      default = "victorialogs";
      description = ''
        Directory below `/var/lib` to store VictoriaLogs log data.
        This directory will be created automatically using systemd's StateDirectory mechanism.
      '';
    };

    retentionPeriod = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "15d";
      description = ''
        How long to retain samples in storage.
        The minimum retentionPeriod is 24h or 1d. See also -retentionFilter
        The following optional suffixes are supported: s (second), h (hour), d (day), w (week), y (year).
        If suffix isn't set, then the duration is counted in months (default 1)
      '';
    };

    extraOptions = mkOption {
      type = types.listOf types.str;
      default = [];
      example = literalExpression ''
        [
          "-httpAuth.username=username"
          "-httpAuth.password=file:///abs/path/to/file"
          "-loggerLevel=WARN"
        ]
      '';
      description = ''
        Extra options to pass to VictoriaLogs. See the docs:
        <https://docs.victoriametrics.com/victorialogs>
        or {command}`victorialogs -help` for more information.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.victorialogs = {
      description = "VictoriaLogs database";
      wantedBy = ["multi-user.target"];
      after = ["network.target"];
      startLimitBurst = 5;

      serviceConfig = {
        ExecStart = lib.escapeShellArgs startCLIList;

        DynamicUser = true;
        RestartSec = 1;
        Restart = "on-failure";
        RuntimeDirectory = "victorialogs";
        RuntimeDirectoryMode = "0700";
        StateDirectory = cfg.stateDir;
        StateDirectoryMode = "0700";

        # Increase the limit to avoid errors like 'too many open files'  when merging small parts
        LimitNOFILE = 1048576;

        # Hardening
        DeviceAllow = ["/dev/null rw"];
        DevicePolicy = "strict";
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        NoNewPrivileges = true;
        PrivateDevices = true;
        PrivateTmp = true;
        PrivateUsers = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        ProtectSystem = "full";
        RemoveIPC = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
          "AF_UNIX"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [
          "@system-service"
          "~@privileged"
        ];
      };

      postStart = let
        bindAddr =
          (lib.optionalString (lib.hasPrefix ":" cfg.listenAddress) "127.0.0.1") + cfg.listenAddress;
      in
        lib.mkBefore ''
          until ${lib.getBin pkgs.curl}/bin/curl -s -o /dev/null http://${bindAddr}/ping; do
            sleep 1;
          done
        '';
    };
  };
}
