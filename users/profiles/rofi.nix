{pkgs, ...}:

{
  programs.rofi = {
    enable = true;
    package = pkgs.rofi-wayland;
    font = "Roboto Medium 14";
    width = 30;
    lines = 6;
    theme = "${pkgs.inputs.rofi-nord-theme}/nord.rasi";
    extraConfig = {
      modi = "run,ssh,drun";
    };
  };
}
