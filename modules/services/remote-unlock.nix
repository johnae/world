{ config, lib, pkgs, ...}:

let
  inherit (builtins) filter getAttr length foldl';
  inherit (lib) mkEnableOption mkOption mkIf;
  inherit (lib.types) port str listOf submodule;
  cfg = config.services.remote-unlock;
  enabledCfgs = filter (getAttr "enable") cfg;
  mkRemoteDiskUnlock = { host, port, passwordFile, identityFile, interval, ... }:
    let
      strPort = toString port;
      name = "remote-unlock-${host}-${strPort}";
    in
  {
    timers.${name} = {
      description = "Try remote unlocking ${host}:${strPort} every ${interval}";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnUnitInactiveSec = interval;
        OnBootSec = interval;
        RandomizedDelaySec = "30s";
      };
    };
    services.${name} = {
      description = "Remote unlock ${host}:${strPort}";
      script = ''
        echo "Probing host ${host} on strPort ${strPort}"
        if timeout 5 ${pkgs.bash}/bin/bash -c "</dev/tcp/${host}/${strPort}"; then
          echo "Host ${host} is listening on strPort ${strPort}, unlocking..."
          cat ${passwordFile} | \
            ${pkgs.openssh}/bin/ssh -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null \
                -i ${identityFile} -p ${strPort} ${host}
        else
          echo "No response on ${host}:${strPort}, going to sleep"
        fi
      '';
    };
  };
  mkRemoteDiskUnlockers = cfgs:
    foldl' (a: b: a // b) {} (map mkRemoteDiskUnlock cfgs);
in
{
  options.services.remote-unlock = with lib; mkOption {
    default = [];
    type = listOf (submodule {
      options = {
        enable = mkEnableOption "remote unlock";
        host = mkOption {
          type = str;
          example = "1.1.1.1";
          description = "The hostname or ip address of the remote host";
        };
        port = mkOption {
          type = port;
          example = "1234";
          description = "The port of the remote host";
        };
        passwordFile = mkOption {
          type = str;
          description = "The path to a file containing the unlock password of remote host disk";
          example = "/path/to/passwordFile";
        };
        identityFile = mkOption {
          type = str;
          description = "The path to a file containing the ssh identity to use when connecting to remote host";
          example = "/path/to/id_ed25519";
        };
        interval = mkOption {
          type = str;
          default = "5m";
          example = "10m";
          description = "How often to run the unlocker. This needs to be a systemd monotonic timer spec.";
        };
      };
    });
  };

  config = mkIf ((length enabledCfgs) > 0) {
    systemd = mkRemoteDiskUnlockers enabledCfgs;
  };
}
