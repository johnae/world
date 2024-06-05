{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkEnableOption mkOption mkIf;
  inherit (lib.types) str;
  cfg = config.services.hcloud-remote-unlock-all;
  hcloud-unlock-all = pkgs.writeShellApplication {
    name = "hcloud-unlock-all";
    runtimeInputs = [pkgs.hcloud pkgs.gawk pkgs.openssh pkgs.bash];
    text = ''
      HCLOUD_TOKEN=$(cat "$1")
      export HCLOUD_TOKEN
      for server in $(hcloud server list -o noheader | awk '{print $4}'); do
        echo "Probing host $server on port 22"
        if timeout 5 bash -c "</dev/tcp/$server/22"; then
          echo "Host $server is already unlocked, skipping"
          continue
        fi
        echo "No response on port 22, probing host $server on port 2222"
        if timeout 5 bash -c "</dev/tcp/$server/2222"; then
          echo "Host $server is waiting for unlock - unlocking"
          ssh -oStrictHostKeyChecking=no -oUserKnownHostsFile=/dev/null -i "$3" -p 2222 "root@$server" < "$2" || true
        else
          echo "Host $server is down, retry later"
        fi
      done
    '';
  };
in {
  options.services.hcloud-remote-unlock-all = {
    enable = mkEnableOption "remote unlock all disks on all hcloud servers";
    diskpasswordFile = mkOption {
      type = str;
      example = "/path/to/diskpassword-file";
      description = "The disk password file";
    };
    hcloudTokenFile = mkOption {
      type = str;
      example = "/path/to/token-file";
      description = "The Hetzner Cloud API token";
    };
    identityFile = mkOption {
      type = str;
      example = "/path/to/private-key";
      description = "The SSH private key file";
    };
  };
  config = mkIf cfg.enable {
    systemd.timers.hcloud-remote-unlock-all = {
      description = "Remote unlock all disks on all Hetzner Cloud servers";
      wantedBy = ["timers.target"];
      timerConfig = {
        OnUnitInactiveSec = "5m";
        OnBootSec = "5m";
        RandomizedDelaySec = "30s";
      };
    };
    systemd.services.hcloud-remote-unlock-all = {
      description = "Remote unlock all disks on all Hetzner Cloud servers";
      after = ["network-online.target"];
      requires = ["network-online.target"];
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${hcloud-unlock-all}/bin/hcloud-unlock-all ${cfg.hcloudTokenFile} ${cfg.diskpasswordFile} ${cfg.identityFile}";
      };
    };
  };
}
