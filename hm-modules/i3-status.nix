{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.i3status-rust;

  configFile = conf:
    pkgs.runCommand "i3status-rust.toml"
      {
        buildInputs = [ pkgs.remarshal ];
        preferLocalBuild = true;
        allowSubstitutes = false;
      }
      ''
        remarshal -if json -of toml \
          < ${pkgs.writeText "config.json" (builtins.toJSON conf)} \
          > $out
      '';
in
{
  options.programs.i3status-rust = {
    enable = mkEnableOption "i3status-rust: Generates status bar for i3bar.";
    settings = {
      block = mkOption {
        description = "List of attribute sets of defined i3status-rust blocks.";
        type = types.listOf types.attrs;
      };
      theme = {
        name = mkOption {
          description = "Theme name";
          type = types.enum [
            "slick"
            "solarized_dark"
            "solarized_light"
            "modern"
            "plain"
            "bad_wolf"
            "gruvbox_light"
            "gruvbox_dark"
          ];
          default = "modern";
        };
        overrides = mkOption {
          description = "Theme color overrides";
          type = types.nullOr (types.attrsOf types.str);
          default = null;
        };
      };
      icons = {
        name = mkOption {
          description = "Icons name";
          type = types.enum [
            "none"
            "awesome"
            "material"
          ];
          default = "awesome";
        };
        overrides = mkOption {
          description = "Icon overrides";
          type = types.nullOr (types.attrsOf types.str);
          default = null;
        };
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.i3status-rust ];
    xdg.configFile."i3status-rust/config".source = configFile cfg.settings;
  };
}
