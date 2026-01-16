{
  pkgs,
  lib,
  config,
  ...
}: let
  inherit (config) home;
  nu-scripts = "${pkgs.nu_scripts}/share/nu_scripts";
  configDir =
    if pkgs.stdenv.isDarwin && !config.xdg.enable
    then "Library/Application Support/nushell"
    else "${config.xdg.configHome}/nushell";
  configPath = path:
    if (builtins.substring 0 1 path) == "/"
    then path
    else "~/${path}";
in {
  home.packages = [
    pkgs.jwt-cli ## see env.nu for impl
  ];
  programs.atuin.enable = false; ## disable for now
  programs.atuin.settings = {
    auto_sync = true;
    sync_frequency = "5m";
    sync_address = "http://atuin-atuin.tail68e9c.ts.net";
    sync.records = true;
  };
  programs.direnv.enableNushellIntegration = false;
  programs.atuin.enableNushellIntegration = false; ## disable for now
  programs.nushell = {
    enable = true;
    package = pkgs.nushell;
    configFile.source = ./config.nu;
    envFile.source = ./env.nu;
    extraConfig = ''
      source "${configPath configDir}/home.nu"
      source "${configPath configDir}/starship.nu"
      ${
        if pkgs.stdenv.isDarwin
        then ''
          $env.PATH = ($env.PATH | split row (char esep) | prepend '/opt/homebrew/bin')
        ''
        else ""
      }

      $env.config.hooks.pre_prompt ++= [
       {
        code: "
          if ('ZELLIJ' in $env) {
            mut current_dir = (pwd)
            if ($current_dir == $env.HOME) {
              $current_dir = '~'
            }
            let current_dir = (echo $current_dir | split row "/" | last)
            zellij action rename-tab $current_dir out+err> /dev/null
          }
        "
       }
      ]

      ${
        lib.concatStringsSep "\n"
        (
          (
            map (completion: "use ${nu-scripts}/custom-completions/${completion}/${completion}-completions.nu") [
              "bat"
              "cargo"
              "curl"
              "gh"
              "git"
              "just"
              "make"
              "man"
              "nix"
              "npm"
              "rg"
              "tar"
              "typst"
              "zellij"
            ]
          )
          ++ [
            "use ${nu-scripts}/custom-completions/yarn/yarn-v4-completions.nu"
          ]
          ++ (
            map (module_path: "source ${nu-scripts}/modules/${module_path}") [
              #              "argx/mod.nu"
              #              "data_extraction/ultimate_extractor.nu"
              # "kubernetes/mod.nu"
              #              "nix/nix.nu"
            ]
          )
        )
      }

      use ${nu-scripts}/custom-completions/yarn/yarn-v4-completions.nu
    '';
  };
  home.file."${configDir}/starship.nu".source = ./starship.nu;
  home.file."${configDir}/home.nu".source = pkgs.writeText "home.nu" ''
    ${
      lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: "$env.${name} = \"${value}\"") home.sessionVariables)
    }
  '';
}
