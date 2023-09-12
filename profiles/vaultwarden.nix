{
  lib,
  config,
  pkgs,
  ...
}: let
  cfg = config.services.vaultwarden;
  user = config.users.users.vaultwarden.name;
  group = config.users.users.vaultwarden.group;
  backup = pkgs.writeText "backup.sh" ''
    ${pkgs.sqlite}/bin/sqlite3 /var/lib/bitwarden_rs/db.sqlite3 "VACUUM INTO '${cfg.backupDir}/db.sqlite3'";
    cp -R /var/lib/bitwarden_rs/{attachments,sends,rsa_key*,icon_cache} "${cfg.backupDir}"/;
  '';
in {
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
    };
    wantedBy = ["multi-user.target"];
  };
}
