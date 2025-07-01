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

  wezterm-test = pkgs.writeShellApplication {
    name = "wezterm-test";
    text = ''
      WEZTERM_FNL="./users/profiles/wezterm/wezterm.fnl" wezterm start --always-new-process "$@"
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

  files.".jj/repo/config.toml".toml = {
    revset-aliases = {
      "trunk()" = "main@origin";
    };
    user = {
      email = "john@insane.se";
      name = "John Axel Eriksson";
    };
  };

  packages = with pkgs; [
    agenix
    age-plugin-yubikey
    alejandra
    bash-language-server
    deadnix
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
    opentofu
    # tofuWithPlugins # broken
    watchexec
    wezterm-test
    world
    zellij
    yj
  ];

  enterShell = ansiEscape ''
     echo -e "
      {bold}{106}Declarative Today. {127}Utopia Tomorrow.{reset}

      All machines, all tweaks, all things, one flake.
    "
  '';
}
