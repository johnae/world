{pkgs, ...}: let
  notmuch = pkgs.writeShellApplication {
    name = "notmuch";
    text = ''
      exec ssh icarus "notmuch $*"
    '';
  };
in {
  home.packages = [
    notmuch
  ];
}
