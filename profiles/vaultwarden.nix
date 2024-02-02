{
  lib,
  config,
  pkgs,
  adminUser,
  ...
}: let
  cfg = config.services.vaultwarden;
  user = config.users.users.vaultwarden.name;
  inherit (config.users.users.vaultwarden) group;
  DATA_FOLDER = "/var/lib/bitwarden_rs";
  backup = pkgs.writeText "backup.sh" ''
    export DIR="${cfg.backupDir}/backup-$(date '+%Y%m%d-%H%M')"
    if [ -d "$DIR" ]; then
      echo "Backup directory $DIR already exists, skipping backup"
      exit 0
    fi
    mkdir -p "$DIR"
    ${pkgs.sqlite}/bin/sqlite3 /var/lib/bitwarden_rs/db.sqlite3 "VACUUM INTO '$DIR/db.sqlite3'";
    cp -R /var/lib/bitwarden_rs/{attachments,sends,rsa_key*,icon_cache} "$DIR"/;
    chown -R ${toString adminUser.uid}:${toString adminUser.gid} "${cfg.backupDir}"
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
  environment.persistence."/keep".directories = [DATA_FOLDER];
  fileSystems."${cfg.backupDir}" = {
    device = "/home/${adminUser.name}/Sync/vaultwarden-backup";
    fsType = "none";
    options = ["bind"];
  };
  systemd.services.restore-vaultwarden-backup = {
    description = "restore vaultwarden backup";
    wantedBy = ["multi-user.target"];
    after = ["local-fs.target"];
    before = ["vaultwarden.service" "backup-vaultwarden.service"];
    serviceConfig = {
      Type = "oneshot";
      User = "root"; ## to change perms on restore
      Group = "root";
      ExecStart = "${pkgs.bash}/bin/bash ${restore-vw-backup}";
    };
  };
  systemd.services.vaultwarden = {
    requires = ["restore-vaultwarden-backup.service"];
    after = ["restore-vaultwarden-backup.service"];
  };
  systemd.services.backup-vaultwarden = lib.mkIf (cfg.backupDir != null) {
    serviceConfig = {
      User = "root"; ## need to change perms etc
      Group = "root";
      ExecStart = lib.mkForce "${pkgs.bash}/bin/bash ${backup}"; ## use a tweaked backup script instead of upstream
    };
  };
}
