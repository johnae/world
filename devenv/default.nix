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
    nil
    just
    world
    statix
  ];

  enterShell = ansiEscape ''
     echo -e "
      {bold}{106}Atomic Worlds. {88}Declarative Today. {127}Utopia Tomorrow.{reset}

      This repo contains all my machine definitions and extra packages I like to keep up-to-date with upstream or that
      have been tweaked somehow.
    "
  '';
}
