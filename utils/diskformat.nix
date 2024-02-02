{
  config,
  lib,
  writeShellApplication,
  keyutils,
  ...
}: let
  bcachefsDisks = config.config.bcachefs.disks;
in
  if builtins.length bcachefsDisks > 0
  then import ./diskformat-bcachefs-dmcrypt.nix {inherit config lib keyutils writeShellApplication;}
  else import ./diskformat-btrfs.nix {inherit config lib writeShellApplication;}
