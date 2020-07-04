{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.rbsnapper;

  pango = attrs: str:
    "<span " + (
      lib.concatStringsSep " "
        (lib.mapAttrsToList (name: value: "${name}='${value}' ") attrs)
    ) + ">"
    + str + "</span>";

  mkRbSnapper = { OnUnitInactiveSec ? "30m", OnBootSec ? "5m", ... }: {
    timers.rbsnapper = {
      description =
        "run rbsnapper every ${OnUnitInactiveSec} and ${OnBootSec} after boot";
      wantedBy = [ "timers.target" ]; # enable it and autostart
      timerConfig = { inherit OnUnitInactiveSec OnBootSec; };
    };

    services.rbsnapper = rec {
      description = "Snapshot and remote backup of /home to ${cfg.destination}";

      preStart = with pkgs; ''
        ${udev}/bin/systemctl set-environment \
          STARTED_AT="$(${coreutils}/bin/date +%s)"
      '';

      script = with pkgs; ''
        ${udev}/bin/systemd-inhibit \
          --what="idle:shutdown:sleep" \
          --who="btr-snap" --why="Backing up /home" --mode=block \
            ${btr-snap}/bin/btr-snap /home \
              ${cfg.destination} ${cfg.port} ${cfg.sshKey}
      '';

      postStop = with pkgs; ''
        if [ -e /run/user/1337/env-vars ]; then
           source /run/user/1337/env-vars
        fi
        PID="$(${procps}/bin/pgrep -u 1337 sway | head -1)"
        if [ -n "$PID" ]; then
          USER_PROCESS_ENV=/proc/"$PID"/environ
          if cat "$USER_PROCESS_ENV" | ${gnugrep}/bin/egrep -z DBUS_SESSION_BUS_ADDRESS; then
             export "$(cat "$USER_PROCESS_ENV" | ${gnugrep}/bin/egrep -z DBUS_SESSION_BUS_ADDRESS)"
          fi
        fi
        export DISPLAY=:0
        ENDED_AT="$(${coreutils}/bin/date +%s)"
        DURATION="$((ENDED_AT - STARTED_AT))"
        NOTIFY="${notify-desktop}/bin/notify-desktop"
        if [ "$EXIT_STATUS" = "0" ]; then
           MSG="${pango { font_weight = "bold"; } "Completed"} ${
      toLower description
      } in $DURATION"s.
           ${busybox}/bin/su "$USER" -s /bin/sh -c \
             "$NOTIFY -i emblem-insync-syncing \"Backup\" \"$MSG\""
        else
           MSG="${pango { font_weight = "bold"; } "Failed"} ${
      toLower description
      } after $DURATION"s.
           ${busybox}/bin/su "$USER" -s /bin/sh -c \
             "$NOTIFY -i dialog-error -u critical \"Backup\" \"$MSG\""
        fi;
      '';

    };

  };
in
{
  options.services.rbsnapper = {

    enable = mkEnableOption
      "enable rbsnapper backup service for storing remote btrfs snapshots.";

    destination = mkOption {
      type = types.str;
      example = "user@example.com";
      description = ''
        SSH user and host for connecting to remote backup storage.
      '';
    };

    sshKey = mkOption {
      type = types.str;
      example = "/path/to/ssh.key";
      default = "/root/.ssh/backup_id_rsa";
      description = ''
        The ssh key to use for connecting to remote.
      '';
    };

    port = mkOption {
      type = types.port;
      example = 4567;
      default = 30022;
      apply = val: toString (val);
      description = ''
        SSH remote port to connect to.
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd = mkRbSnapper { inherit (cfg) destination port sshKey; };
  };

}
