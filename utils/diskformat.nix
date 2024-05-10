{
  config,
  lib,
  writeShellApplication,
  keyutils,
  adminUser,
  ...
}: let
  bcachefsDisks = config.config.bcachefs.disks;
  userFiles =
    if adminUser != null
    then ''
      mkdir -p /mnt/keep/home/${adminUser.name}/.config/nushell
      mkdir -p /mnt/keep/home/${adminUser.name}/.kube
      mkdir -p /mnt/keep/home/${adminUser.name}/.ssh
      touch /mnt/keep/home/${adminUser.name}/.config/nushell/history.txt
      touch /mnt/keep/home/${adminUser.name}/.config/tenacity.cfg
      touch /mnt/keep/home/${adminUser.name}/.kube/config
      touch /mnt/keep/home/${adminUser.name}/.spotify_token_cache.json
      touch /mnt/keep/home/${adminUser.name}/.ssh/known_hosts
      chown -R ${adminUser.uid}:users /mnt/keep/home/${adminUser.name}
    ''
    else "";
in ''
  ${
    if builtins.length bcachefsDisks > 0
    then import ./diskformat-bcachefs-dmcrypt.nix {inherit config lib keyutils writeShellApplication;}
    else import ./diskformat-btrfs.nix {inherit config lib writeShellApplication;}
  }
  ${userFiles}
''
