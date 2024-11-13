{
  pkgs,
  ansiEscape,
  ...
}: let
  project-build = pkgs.writeShellApplication {
    name = "project-build";
    runtimeInputs = [pkgs.watchexec];
    text = ''
      watchexec -- 'world lint; world dead; world dscheck'
    '';
  };

  jjpr = pkgs.writeShellApplication {
    name = "jjpr";
    text = ''
      REV="''${1:-}"
      if [ -z "$REV" ]; then
        echo Please provide the revision id
        exit 1
      fi
      jj git push -c "$REV"
      shift
      GIT_BRANCH="$(jj bookmark list | grep push-"$REV" | awk -F: '{print $1}')"
      gh pr create -H "$GIT_BRANCH" -f
      gh pr merge "$GIT_BRANCH" -s "$@"
    '';
  };
in {
  name = "world";

  packages = with pkgs; [
    agenix
    age-plugin-yubikey
    alejandra
    bash-language-server
    fennel
    fennel-ls
    fnlfmt
    hcloud
    jjpr
    just
    lua
    lua-language-server
    nil
    project-build
    rage
    statix
    stylua
    tofuWithPlugins
    watchexec
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
