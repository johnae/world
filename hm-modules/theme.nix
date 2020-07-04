{ config, lib, pkgs, options, ... }:

with lib;
let
  cfg = config.base16-theme;
  cnotation = replaceStrings [ "#" ] [ "0x" ];
  color = default: mkOption { inherit default; type = types.str; };
  alpha = clr: a: "${clr}${a}";
in
{
  options.base16-theme = {
    enable = mkEnableOption "Enable base16 theme systemwide";
    base00 = color "#2E3440"; # polar night
    base01 = color "#3B4252"; # polar night
    base02 = color "#434C5E"; # polar night
    base03 = color "#4C566A"; # polar night
    base04 = color "#D8DEE9"; # snow storm
    base05 = color "#E5E9F0"; # snow storm
    base06 = color "#ECEFF4"; # snow storm
    base07 = color "#8FBCBB"; # frost
    base08 = color "#88C0D0"; # frost
    base09 = color "#81A1C1"; # frost
    base0A = color "#5E81AC"; # frost
    base0B = color "#BF616A"; # aurora
    base0C = color "#D08770"; # aurora
    base0D = color "#EBCB8B"; # aurora
    base0E = color "#A3BE8C"; # aurora
    base0F = color "#B48EAD"; # aurora
  };

  config = mkIf cfg.enable (
    mkMerge [
      ({
        wayland.windowManager.sway.config.colors = rec {
          focused = {
            border = cfg.base0A;
            background = cfg.base0A;
            text = cfg.base06;
            indicator = cfg.base0A;
            childBorder = cfg.base0A;
          };

          focusedInactive = {
            border = cfg.base00;
            background = cfg.base00;
            text = cfg.base07;
            indicator = cfg.base00;
            childBorder = cfg.base00;
          };

          unfocused = focusedInactive;

          urgent = {
            border = cfg.base0B;
            background = cfg.base0B;
            text = cfg.base05;
            indicator = cfg.base0B;
            childBorder = cfg.base0B;
          };
        };

        programs.alacritty.settings.colors = {
          primary.background = "0x00374e"; ## special - not part of theme
          primary.foreground = "0xD8DEE9";

          cursor.text = cnotation cfg.base00;
          cursor.cursor = cnotation cfg.base04;

          normal.black = cnotation cfg.base01;
          normal.red = cnotation cfg.base0B;
          normal.green = cnotation cfg.base0E;
          normal.yellow = cnotation cfg.base0D;
          normal.blue = cnotation cfg.base09;
          normal.magenta = cnotation cfg.base0F;
          normal.cyan = cnotation cfg.base08;
          normal.white = cnotation cfg.base05;

          bright.black = cnotation cfg.base03;
          bright.red = cnotation cfg.base0B;
          bright.green = cnotation cfg.base0E;
          bright.yellow = cnotation cfg.base0D;
          bright.blue = cnotation cfg.base09;
          bright.magenta = cnotation cfg.base0F;
          bright.cyan = cnotation cfg.base07;
          bright.white = cnotation cfg.base06;
        };

        programs.i3status-rust.settings.theme.overrides = {
          idle_bg = alpha cfg.base03 "DD";
          idle_fg = cfg.base05;

          info_bg = alpha cfg.base06 "DD";
          info_fg = cfg.base00;

          good_bg = alpha cfg.base09 "DD";
          good_fg = cfg.base00;

          warning_bg = alpha cfg.base0D "DD";
          warning_fg = cfg.base00;

          critical_bg = alpha cfg.base0B "DD";
          critical_fg = cfg.base04;
        };
      })
    ]
  );

}
