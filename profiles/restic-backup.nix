{
  adminUser,
  config,
  ...
}: {
  age.secrets = {
    restic-env = {
      file = ../secrets/restic.age;
      owner = "1337";
    };
    restic-pw = {
      file = ../secrets/restic-pw.age;
      owner = "1337";
    };
  };

  services.restic = {
    backups = {
      remote = {
        paths = [
          "/home/${adminUser.name}/Development"
          "/home/${adminUser.name}/Documents"
          "/home/${adminUser.name}/Sync"
          "/home/${adminUser.name}/Photos"
        ];
        environmentFile = config.age.secrets.restic-env.path;
        passwordFile = config.age.secrets.restic-pw.path;
        repository = "s3:https://b0850d27a4d43d7d0f8d36ebc6a1bfab.r2.cloudflarestorage.com/restic-9000-b147";
        initialize = true;
        timerConfig.OnCalendar = "*-*-* *:00:00";
        timerConfig.RandomizedDelaySec = "5m";
        extraBackupArgs = [
          "--exclude=\".direnv\""
          "--exclude=\".terraform\""
          "--exclude=\"node_modules/*\""
        ];
      };
    };
  };
}
