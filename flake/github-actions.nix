{
  inputs,
  withSystem,
  lib,
  self,
  ...
}: let
  inherit (lib) mapAttrsToList filterAttrs hasPrefix filter elem;
  defaultSkip = [
    "nu-cli"
    "nu-color-config"
    "nu-command"
    "nu-engine"
    "nu-explore"
    "nu-glob"
    "nu-json"
    "nu-parser"
    "nu-path"
    "nu-plugin"
    "nu-pretty-hex"
    "nu-protocol"
    "nu-system"
    "nu-table"
    "nu-term-grid"
    "nu-test-support"
    "nu-utils"
    "nu_plugin_custom_values"
    "nu_plugin_example"
    "nu_plugin_gstat"
    "nu_plugin_inc"
    "nu_plugin_query"
  ];
in {
  flake = {
    github-actions-package-matrix-x86-64-linux = withSystem "x86_64-linux" (ctx @ {pkgs, ...}: let
      skip =
        (mapAttrsToList (name: _: name) (filterAttrs (name: _: hasPrefix "images/" name) pkgs))
        ++ defaultSkip;
    in {
      os = ["ubuntu-latest"];
      pkg = filter (item: !(elem item skip)) (mapAttrsToList (name: _: name) ctx.config.packages);
    });

    github-actions-package-matrix-aarch64-linux = withSystem "aarch64-linux" (ctx @ {pkgs, ...}: let
      skip =
        (mapAttrsToList (name: _: name) (filterAttrs (name: _: hasPrefix "images/" name) pkgs))
        ++ defaultSkip
        ++ [
          "age-plugin-yubikey"
          "blur"
          "fire"
          "git-branchless"
          "grim"
          "kanshi"
          "kile"
          "matrix-conduit"
          "my-emacs"
          "my-emacs-config"
          "mynerdfonts"
          "netns-dbus-proxy"
          "netns-exec"
          "nixpkgs-fmt"
          "nushell"
          "persway"
          "pixieboot"
          "pxebooter"
          "ristate"
          "rofi-wayland"
          "rust-analyzer-bin"
          "scripts"
          "slurp"
          "spotifyd"
          "spotnix"
          "sway"
          "sway-unwrapped"
          "swaybg"
          "swayidle"
          "swaylock"
          "swaylock-dope"
          "waybar"
          "wayland-protocols-master"
          "wayland120"
          "wl-cliboard"
          "wl-cliboard-x11"
          "wlroots-master"
          "xdg-desktop-portal-wlr"
        ];
    in {
      os = ["ubuntu-latest"];
      pkg = filter (item: !(elem item skip)) (mapAttrsToList (name: _: name) ctx.config.packages);
    });

    github-actions-host-matrix-x86-64-linux = {
      os = ["9k"];
      host = mapAttrsToList (name: _: name) (filterAttrs (_: config: config.pkgs.system == "x86_64-linux") self.nixosConfigurations);
    };

    github-actions-host-matrix-aarch64-linux = {
      os = ["9k"];
      host = mapAttrsToList (name: _: name) (filterAttrs (_: config: config.pkgs.system == "aarch64-linux") self.nixosConfigurations);
    };
  };
}
