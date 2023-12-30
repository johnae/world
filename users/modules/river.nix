{
  pkgs,
  config,
  lib,
  ...
}: let
  l = lib // builtins;
  inherit
    (l)
    mkOption
    mkIf
    mkEnableOption
    optional
    types
    flatten
    mapAttrsToList
    isList
    isAttrs
    ;

  cfg = config.wayland.windowManager.river;

  mapSettings = s: let
    mapField = path: value:
      if isAttrs value
      then
        mapAttrsToList (
          k: v:
            if isAttrs v
            then mapField "${path} ${k}" v
            else if isList v
            then "riverctl ${path} ${k} spawn '${lib.concatStringsSep ";" (map (c: "riverctl ${c}") v)}'"
            else "riverctl ${path} ${k} ${v}"
        )
        value
      else if isList value
      then lib.concatStringsSep "\n" (map (v: "riverctl ${path} ${v}") value)
      else "riverctl ${path} ${toString value}";
  in
    flatten (mapAttrsToList mapField (lib.filterAttrs (
        k: v:
          k
          != "additional-modes"
          && k != "layout-generator-exec"
          && k != "exec"
      )
      s));

  writeConfig = conf: let
    inherit (conf) settings;
  in
    pkgs.writeShellApplication {
      name = "init";
      runtimeInputs = [cfg.package];
      text = ''
        set -x

        ${
          lib.concatStringsSep "\n" (map (mode: "riverctl ${mode}") settings.additional-modes)
        }

        ${
          lib.concatStringsSep "\n" (mapSettings settings)
        }

        for i in $(seq 1 9)
        do
            tags=$((1 << (i - 1)))
            riverctl map normal Super "$i" set-focused-tags $tags
            riverctl map normal Super+Shift "$i" set-view-tags $tags
            riverctl map normal Super+Control "$i" toggle-focused-tags $tags
            riverctl map normal Super+Shift+Control "$i" toggle-view-tags $tags
        done

        all_tags=$(((1 << 32) - 1))
        riverctl map normal Super 0 set-focused-tags "$all_tags"
        riverctl map normal Super+Shift 0 set-view-tags "$all_tags"

        for mode in normal locked
        do
            riverctl map $mode None XF86Eject spawn 'eject -T'

            riverctl map $mode None XF86AudioRaiseVolume  spawn 'pamixer -i 5'
            riverctl map $mode None XF86AudioLowerVolume  spawn 'pamixer -d 5'
            riverctl map $mode None XF86AudioMute         spawn 'pamixer --toggle-mute'

            riverctl map $mode None XF86AudioMedia spawn 'playerctl play-pause'
            riverctl map $mode None XF86AudioPlay  spawn 'playerctl play-pause'
            riverctl map $mode None XF86AudioPrev  spawn 'playerctl previous'
            riverctl map $mode None XF86AudioNext  spawn 'playerctl next'

            riverctl map $mode None XF86MonBrightnessUp   spawn 'light -A 5'
            riverctl map $mode None XF86MonBrightnessDown spawn 'light -U 5'
        done

        ${
          if cfg.systemd.enable
          then ''
            ${pkgs.dbus}/bin/dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY XDG_CURRENT_DESKTOP || true
            systemctl --user start river-session.target
          ''
          else ""
        }

        ${
          lib.concatStringsSep "\n" (map (exec: "riverctl spawn '${exec}'") settings.exec)
        }

        riverctl spawn '${settings.layout-generator-exec}'

        ${pkgs.xorg.xrdb}/bin/xrdb -merge ~/.Xresources || true
        ${pkgs.gnome.gnome-settings-daemon}/libexec/gsd-xsettings || true

      '';
    };
in {
  options.wayland.windowManager.river = {
    enable = mkEnableOption "Enable the river compositor";
    package = mkOption {
      type = types.package;
      default = pkgs.river;
    };
    systemd = {
      enable = mkOption {
        type = types.bool;
        default = pkgs.stdenv.isLinux;
      };
      xdgAutostart = mkEnableOption "Enable xdg-desktop-autostart.target";
    };
    # xkb = {
    #   default_layout = mkOption {
    #     type = types.str;
    #     default = "us";
    #   };
    #   default_options = mkOption {
    #     type = types.str;
    #     default = "";
    #   };
    #   default_model = mkOption {
    #     type = types.str;
    #     default = "pc105";
    #   };
    #   default_variant = mkOption {
    #     type = types.str;
    #     default = "";
    #   };
    # };
    # xkb_default_layout = mkOption {
    #   type = types.str;
    #   default = "us";
    # };
    settings = mkOption {
      default = {};
      type = types.submodule {
        freeformType = types.attrsOf types.raw;
        options.additional-modes = mkOption {
          type = types.listOf types.str;
          default = [];
          apply = map (mode: "declare-mode ${mode}");
        };
        options.float-filters = mkOption {
          type = types.attrsOf (types.listOf types.str);
          default = {};
          apply = filters: flatten (mapAttrsToList (target: matchers: (map (matcher: "float-filter-add ${target} \"${matcher}\"") matchers)) filters);
        };
        options.csd-filters = mkOption {
          type = types.attrsOf (types.listOf types.str);
          default = {};
          apply = filters: flatten (mapAttrsToList (target: matchers: (map (matcher: "float-filter-add ${target} \"${matcher}\"") matchers)) filters);
        };
        options.layout-generator-exec = mkOption {
          type = types.str;
          default = "${pkgs.river}/bin/rivertile -view-padding 4 -outer-padding 4";
        };
        options.exec = mkOption {
          type = types.listOf types.str;
          default = [];
        };
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.user.targets.river-session = mkIf cfg.systemd.enable {
      Unit = {
        Description = "river compositor session";
        Documentation = ["man:systemd.special(7)"];
        BindsTo = ["graphical-session.target"];
        Wants =
          ["graphical-session-pre.target"]
          ++ optional cfg.systemd.xdgAutostart "xdg-desktop-autostart.target";
        After = ["graphical-session-pre.target"];
        Before =
          optional cfg.systemd.xdgAutostart "xdg-desktop-autostart.target";
      };
    };
    xdg.configFile."river/init".source = "${writeConfig cfg}/bin/init";
    # home.sessionVariables = {
    #   XKB_DEFAULT_LAYOUT = cfg.xkb.default_layout;
    #   XKB_DEFAULT_OPTIONS = cfg.xkb.default_options;
    #   XKB_DEFAULT_MODEL = cfg.xkb.default_model;
    #   XKB_DEFAULT_VARIANT = cfg.xkb.default_variant;
    # };
    home.packages = [cfg.package];
  };
}
