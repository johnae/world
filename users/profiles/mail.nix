{
  pkgs,
  lib,
  config,
  ...
}: {
  home.packages = with pkgs; [
    muchsync
  ];
  programs.msmtp.enable = true;
  programs.notmuch.enable = true;
  services.muchsync.remotes.icarus = {
    frequency = "*:0/5";
    upload = false;
    remote.host = "icarus";
    remote.importNew = false;
    local.importNew = false;
  };

  systemd.user.services.muchsync-icarus = with lib; let
    remoteCfg = config.services.muchsync.remotes.icarus;
  in {
    Service.ExecStart = lib.mkForce (concatStringsSep " " (["${pkgs.muchsync}/bin/muchsync"]
      ++ ["-s ${escapeShellArg remoteCfg.sshCommand}"]
      ++ optional (!remoteCfg.upload) "--noup"
      # local configuration
      ++ optional remoteCfg.local.checkForModifiedFiles "-F"
      ++ optional (!remoteCfg.local.importNew) "--nonew"
      # remote configuration
      ++ [(escapeShellArg remoteCfg.remote.host)]
      ++ optional (remoteCfg.remote.muchsyncPath != "")
      "-r ${escapeShellArg remoteCfg.remote.muchsyncPath}"
      ++ optional remoteCfg.remote.checkForModifiedFiles "-F"
      ++ optional (!remoteCfg.remote.importNew) "--nonew"
      ++ ["-C ${config.home.sessionVariables.NOTMUCH_CONFIG}"]));
  };
}
