{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  cfg = config.system.autoUpgrade;
in {
  options = {
    system.autoUpgrade = {
      enableSentinel = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable a sentinel file signaling when a reboot is needed.
          If enabled, instead of rebooting a file is created in <literal>/var/run/reboot-required</literal>
          which can be used by external tooling to reboot the machine at a proper
          time.
          Must be used together with setting the option <option>system.autoUpgrade.allowReboot</option>
          to true.
        '';
      };
    };
  };

  config = lib.mkIf (cfg.enable && cfg.enableSentinel && cfg.allowReboot) {
    systemd.services.nixos-upgrade = {
      script = let
        nixos-rebuild = "${config.system.build.nixos-rebuild}/bin/nixos-rebuild";
        readlink = "${pkgs.coreutils}/bin/readlink";
        upgradeFlag = optional (cfg.channel == null) "--upgrade";
      in
        lib.mkForce ''
          ${nixos-rebuild} boot ${toString (cfg.flags ++ upgradeFlag)}
          booted="$(${readlink} /run/booted-system/{initrd,kernel,kernel-modules})"
          built="$(${readlink} /nix/var/nix/profiles/system/{initrd,kernel,kernel-modules})"

          if [ "''${booted}" = "''${built}" ]; then
            ${nixos-rebuild} switch ${toString cfg.flags}
          else
            ## signal that a reboot is needed
            touch /var/run/reboot-required
          fi
        '';
    };
  };
}
