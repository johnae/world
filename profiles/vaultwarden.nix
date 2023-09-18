{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.services.vaultwarden;
  user = config.users.users.vaultwarden.name;
  inherit (config.users.users.vaultwarden) group;
  DATA_FOLDER = "/var/lib/bitwarden_rs";
  backup = pkgs.writeText "backup.sh" ''
    export DIR="${cfg.backupDir}/backup-$(date '+%Y%m%d-%H%M')"
    mkdir -p "$DIR"
    ${pkgs.sqlite}/bin/sqlite3 /var/lib/bitwarden_rs/db.sqlite3 "VACUUM INTO '$DIR/db.sqlite3'";
    cp -R /var/lib/bitwarden_rs/{attachments,sends,rsa_key*,icon_cache} "$DIR"/;
    chown -R 1337 "$DIR"
    echo removing old backups
    (cd "${cfg.backupDir}";
      for dir in $(ls -r | tail -n +10); do
        echo removing backup directory "$dir"
        rm -rf "$dir"
      done
    )
  '';
  restore-vw-backup = pkgs.writeText "restore-vw-backup.sh" ''
    BACKUP="$(ls "${cfg.backupDir}"/ | tail -n1)"
    echo possibly restoring from latest backup "$BACKUP"
    if [ ! -e "${DATA_FOLDER}/db.sqlite3" ] && [ ! -z "$BACKUP" ]; then
      cp -R ${cfg.backupDir}/"$BACKUP"/* ${DATA_FOLDER}/
      chown -R ${user}:${group} ${DATA_FOLDER}
    else
      echo skipping restore
    fi
  '';
in {
  fileSystems."${cfg.backupDir}" = {
    device = "/home/john/Sync/vaultwarden-backup";
    fsType = "none";
    options = ["bind"];
  };
  systemd.services.vaultwarden.serviceConfig.ExecStartPre = "${pkgs.bash}/bin/bash ${restore-vw-backup}";
  systemd.timers.backup-vaultwarden = lib.mkIf (cfg.backupDir != null) {
    timerConfig.OnCalendar = "0/12:00";
  };
  systemd.services.backup-vaultwarden = lib.mkForce {
    description = "Backup vaultwarden";
    environment = {
      DATA_FOLDER = "/var/lib/bitwarden_rs";
      BACKUP_FOLDER = cfg.backupDir;
    };
    path = with pkgs; [sqlite];
    # if both services are started at the same time, vaultwarden fails with "database is locked"
    before = ["vaultwarden.service"];
    serviceConfig = {
      SyslogIdentifier = "backup-vaultwarden";
      Type = "oneshot";
      User = lib.mkDefault user;
      Group = lib.mkDefault group;
      ExecStart = "${pkgs.bash}/bin/bash ${backup}";
      RemainAfterExit = "yes";
    };
    wantedBy = ["multi-user.target"];
  };
}
