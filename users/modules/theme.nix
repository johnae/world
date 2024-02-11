{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (lib) mkOption mkMerge mkIf mkEnableOption types;
  cfg = config.base16-theme;
  cnotation = builtins.replaceStrings ["#"] ["0x"];
  footclr = builtins.replaceStrings ["#"] [""];
  fuzzelclr = clr: alpha: "${footclr clr}${alpha}";
  color = default:
    mkOption {
      inherit default;
      type = types.str;
    };
  alpha = clr: a: "${clr}${a}";
in {
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
      {
        wayland.windowManager.sway.config.colors = {
          focused = {
            border = cfg.base0A;
            background = cfg.base0A;
            text = cfg.base06;
            indicator = cfg.base0A;
            childBorder = cfg.base0A;
          };

          focusedInactive = {
            border = cfg.base03;
            background = cfg.base03;
            text = cfg.base04;
            indicator = cfg.base00;
            childBorder = cfg.base00;
          };

          unfocused = {
            border = cfg.base00;
            background = cfg.base00;
            text = cfg.base0F;
            indicator = cfg.base00;
            childBorder = cfg.base00;
          };

          urgent = {
            border = cfg.base0B;
            background = cfg.base0B;
            text = cfg.base05;
            indicator = cfg.base0B;
            childBorder = cfg.base0B;
          };
        };

        programs.foot.settings = {
          cursor = {
            color = "${footclr cfg.base00} ${footclr cfg.base04}";
          };

          colors = {
            background = "00374e"; ## special - not part of theme
            foreground = footclr cfg.base04;

            regular0 = footclr cfg.base01;
            regular1 = footclr cfg.base0B;
            regular2 = footclr cfg.base0E;
            regular3 = footclr cfg.base0D;
            regular4 = footclr cfg.base09;
            regular5 = footclr cfg.base0F;
            regular6 = footclr cfg.base08;
            regular7 = footclr cfg.base05;
            bright0 = footclr cfg.base03;
            bright1 = footclr cfg.base0B;
            bright2 = footclr cfg.base0E;
            bright3 = footclr cfg.base0D;
            bright4 = footclr cfg.base09;
            bright5 = footclr cfg.base0F;
            bright6 = footclr cfg.base07;
            bright7 = footclr cfg.base06;
            dim0 = footclr "373e4d";
            dim1 = footclr "94545d";
            dim2 = footclr "809575";
            dim3 = footclr "b29e75";
            dim4 = footclr "68809a";
            dim5 = footclr "8c738c";
            dim6 = footclr "6d96a5";
            dim7 = footclr "aeb3bb";
          };
        };

        programs.alacritty.settings.colors = {
          primary.background = "0x00374e"; ## special - not part of theme
          primary.foreground = cnotation cfg.base04;

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

        #   background = "282a36dd";
        #   text = "f8f8f2ff";
        #   match = "8be9fdff";
        #   selection-match = "8be9fdff";
        #   selection = "44475add";
        #   selection-text = "f8f8f2ff";
        #   border = "bd93f9ff";

        programs.fuzzel.settings.colors = {
          background = fuzzelclr cfg.base00 "dd";
          text = fuzzelclr cfg.base06 "ff";
          match = fuzzelclr cfg.base04 "ff";
          selection-match = fuzzelclr cfg.base04 "ff";
          selection = fuzzelclr cfg.base08 "dd";
          selection-text = fuzzelclr cfg.base06 "ff";
          border = fuzzelclr cfg.base08 "ff";
        };

        xdg.configFile."rio/themes/default.toml".source = (pkgs.formats.toml {}).generate "rio-default-theme" {
          colors = {
            background = "#00374e";
            foreground = "#d8dee9";
            selection-background = "#eceff4";
            selection-foreground = "#4c566a";
            cursor = "#eceff4";
            black = "#3b4252";
            red = "#bf616a";
            green = "#a3be8c";
            yellow = "#ebcb8b";
            blue = "#81a1c1";
            magenta = "#b48ead";
            cyan = "#88c0d0";
            white = "#e5e9f0";
            light_black = "#4c566a";
            light_red = "#bf616a";
            light_green = "#a3be8c";
            light_yellow = "#ebcb8b";
            light_blue = "#81a1c1";
            light_magenta = "#b48ead";
            light_cyan = "#8fbcbb";
            light_white = "#eceff4";
          };
        };

        programs.i3status-rust.bars.default.settings.theme.overrides = {
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
      }
    ]
  );
}
