{
  config,
  lib,
  writeShellApplication,
  keyutils,
  ...
}: let
  bcachefsDisks = config.config.bcachefs.disks;
  adminUser = config.config.adminUser;
  adminUserFiles = config.config.environment.persistence."/keep".users.${adminUser.name}.files;
  userFiles =
    if adminUser != {}
    then ''
      ${lib.concatStringsSep "\n" (map (f: "mkdir -p /mnt/keep${f.parentDirectory.dirPath}; touch /mnt/keep${f.filePath}") adminUserFiles)}
      chown -R ${toString adminUser.uid}:users /mnt/keep/home/${adminUser.name}
    ''
    else "";
in
  writeShellApplication {
    name = "diskformat";
    runtimeInputs = [keyutils];
    text = ''
      ${
        if builtins.length bcachefsDisks > 0
        then import ./diskformat-bcachefs-dmcrypt.nix {inherit config lib keyutils writeShellApplication;}
        else import ./diskformat-btrfs.nix {inherit config lib writeShellApplication;}
      }/bin/diskformat
      ${userFiles}
    '';
  }
