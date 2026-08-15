{
  lib,
  pkgs,
  config,
  ...
}: let
  inherit (config) userinfo;

  # rbw passes --ttyname whenever the calling client has a terminal, so a
  # terminal caller gets its prompt on its own tty and a GUI caller gets a
  # dialog. Display variables can't be used for this: a zellij pane keeps the
  # server's environment, so it still advertises a Wayland display when you
  # reattach over ssh, and the prompt then opens on the unattended screen.
  pinentry-auto = pkgs.writeShellApplication {
    name = "pinentry-auto";
    text = ''
      for arg in "$@"; do
        case $arg in
          --ttyname | -T)
            exec ${lib.getExe' pkgs.pinentry-curses "pinentry-curses"} "$@"
            ;;
        esac
      done
      exec ${lib.getExe' pkgs.pinentry-gnome3 "pinentry-gnome3"} "$@"
    '';
  };
in {
  programs.rbw.enable = true;
  programs.rbw.settings = {
    inherit (userinfo) email;
    lock_timeout = 3600 * 10;
    pinentry =
      if pkgs.stdenv.hostPlatform.isLinux
      then pinentry-auto
      else pkgs.pinentry_mac;
    base_url = "https://bw.9000.dev";
  };
}
