{
  pkgs,
  lib,
  ...
}: let
  # Nix strings only support \t, \r and \n as escape codes, so actually store
  # the literal escape "ESC" code.
  inherit (builtins) foldl' genList;
  esc = "";
  ansiTable =
    {
      reset = "${esc}[0m";
      bold = "${esc}[1m";
      italic = "${esc}[3m";
      underline = "${esc}[4m";
    }
    // (foldl' (x: y: x // {"${toString y}" = "${esc}[38;5;${toString y}m";}) {} (genList (x: x) 256));

  ansiEscape =
    replaceStrings
    (map (key: "{${key}}") (attrNames ansiTable))
    (attrValues ansiTable);
in {
  languages.rust.enable = true;
  pre-commit.hooks = {
    clippy.enable = true;
    rustfmt.enable = true;
  };

  packages = with pkgs; [
    agenix
    alejandra
    yj
    rage
    age-plugin-yubikey
    pixieboot
    lint
    world-updaters
  ];

  enterShell = ansiEscape ''
    cat<<EOF
     {bold}{106}The World{reset}

     This repo contains all my machine definitions and extra packages I like to keep up-to-date with upstream or that
     have been tweaked somehow.

     $(type -p menu &>/dev/null && menu)
    EOF
  '';
}
