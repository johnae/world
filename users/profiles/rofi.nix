{pkgs, ...}:

let
  theme = pkgs.runCommand "nord-theme-rofi" {} ''
      mkdir -p $out
      cat ${pkgs.inputs.rofi-nord-theme}/nord.rasi | \
        tail +24 > $out/nord.rasi
    '';
in

{
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
    font = "Roboto Medium 14";
    width = 50;
    lines = 10;
    theme = "${theme}/nord.rasi";
    extraConfig = {
      modi = "run,ssh,drun";
      display-ssh = "";
      display-run = "";
      display-drun = "";
      display-combi = "";
      show-icons = true;
      line-margin = 10;
      columns = 2;
    };
  };
}
