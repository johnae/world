{
  pkgs,
  ansiEscape,
  ...
}: {
  name = "world";

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
     echo -e "
      {bold}{106}The World{reset}

      This repo contains all my machine definitions and extra packages I like to keep up-to-date with upstream or that
      have been tweaked somehow.
    "
  '';
}
