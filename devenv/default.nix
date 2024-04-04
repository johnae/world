{
  pkgs,
  lib,
  ansiEscape,
  ...
}: let
  tofuProvider = provider:
    provider.override (oldArgs: {
      provider-source-address =
        lib.replaceStrings
        ["https://registry.terraform.io/providers"]
        ["registry.opentofu.org"]
        oldArgs.homepage;
    });
  tofuWithPlugins = pkgs.opentofu.withPlugins (
    p:
      map tofuProvider [p.null p.external p.hcloud p.cloudflare p.random]
  );
in {
  name = "world";

  packages = with pkgs; [
    agenix
    age-plugin-yubikey
    alejandra
    hcloud
    just
    nil
    rage
    statix
    tofuWithPlugins
    world
    yj
  ];

  enterShell = ansiEscape ''
     echo -e "
      {bold}{106}Declarative Today. {127}Utopia Tomorrow.{reset}

      This repo contains all my machine definitions and extra packages I like to keep up-to-date with upstream or that
      have been tweaked somehow.
    "
  '';
}
