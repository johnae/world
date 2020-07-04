{ config, lib, pkgs, ... }:
## This requires certain things from your drive partitioning
## see ../installer/install.sh
let
  cfg = config.boot.btrfsCleanBoot;
in
with lib; {

  options.boot.btrfsCleanBoot = {
    enable =
      mkEnableOption
        "enable the cleanboot option to erase your / and /var etc on every boot";

    wipe = mkOption {
      type = types.listOf types.str;
      example = [ "@" "@var" ];
      description = ''
        Btrfs subvolumes to wipe on boot
      '';
    };

    keep = mkOption {
      type = types.listOf types.str;
      example = [ "/var/lib/bluetooh" "/var/lib/iwd" "/var/lib/docker" ];
      description = ''
        Paths to keep from being wiped on boot
      '';
    };

  };

  config = mkIf cfg.enable {

    systemd.services.cleanboot-before-hibernate =
      {
        description = "Before Hibernate cleanboot";
        wantedBy = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" "suspend-then-hibernate.target" ];
        before = [ "suspend.target" "hibernate.target" "hybrid-sleep.target" "suspend-then-hibernate.target" ];
        script =
          ''
            echo Creating /nowipe
            touch /nowipe
          '';
        serviceConfig.Type = "oneshot";
      };

    systemd.services.cleanboot-after-hibernate =
      {
        description = "After Hibernate cleanboot";
        wantedBy = [ "hibernate.target" "hybrid-sleep.target" "suspend-then-hibernate.target" ];
        after = [ "hibernate.target" "hybrid-sleep.target" "suspend-then-hibernate.target" ];
        script =
          ''
            echo Removing /nowipe
            rm -f /nowipe
          '';
        serviceConfig.Type = "oneshot";
      };

    boot.initrd.postDeviceCommands = lib.mkAfter ''
      mkdir -p /mnt
      mount -o rw,noatime,compress=zstd,ssd,space_cache /dev/disk/by-label/root /mnt
      if test -e /mnt/@/nowipe; then
        echo Not wiping ephemeral data as /nowipe was detected
        rm -f /mnt/@/nowipe
      else
        echo Wiping ephemeral data
        ${lib.concatStringsSep "\n" (
        map (vol: ''
            for vol in $(find "/mnt/${vol}" -depth -inum 256)
            do
              echo Deleting subvolume "$vol"
              btrfs sub delete "$vol"
            done
            echo Creating subvolume "$vol" from /mnt/@blank snapshot
            btrfs sub snapshot /mnt/@blank /mnt/${vol}
          ''
            ) cfg.wipe
        )}

        ${lib.concatStringsSep "\n" (
        map (m:
            let
            dir = builtins.dirOf m;
            in
            ''
              if [ ! -e "/mnt/@keep${m}" ]; then
                echo Creating subvolume "/keep${m}"
                mkdir -p "/mnt/@keep${dir}"
                btrfs sub create "/mnt/@keep${m}"
              fi
            ''
            ) cfg.keep
        )}
      fi
    '';

    systemd.mounts =
      map
        (where:
          {
            before = [ "local-fs.target" ];
            wantedBy = [ "local-fs.target" ];
            what = "/keep${where}";
            inherit where;
            type = "none";
            options = "bind";
          }
        )
        cfg.keep;
  };

}
