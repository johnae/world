{
  pkgs,
  lib,
  config,
  ...
}: let
  inherit (config) home;
  nu-scripts = "${pkgs.nu_scripts}/share/nu_scripts";
in {
  home.packages = [
    pkgs.jwt-cli ## see env.nu for impl
  ];
  programs.atuin.enable = true;
  programs.atuin.settings = {
    auto_sync = true;
    sync_frequency = "5m";
    sync_address = "http://atuin-atuin.tail68e9c.ts.net";
    sync.records = true;
  };
  programs.direnv.enableNushellIntegration = false;
  programs.atuin.enableNushellIntegration = true;
  programs.nushell = {
    enable = true;
    package = pkgs.nushell;
    configFile.source = ./config.nu;
    envFile.source = ./env.nu;
    extraConfig = ''
      source ~/.config/nushell/home.nu
      source ~/.config/nushell/starship.nu

      $env.config.hooks.pre_prompt = [
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

      $env.config.hooks.pre_prompt = (
        $env.config.hooks.pre_prompt | append (source ${nu-scripts}/nu-hooks/nu-hooks/direnv/config.nu)
      )

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
              "argx/mod.nu"
              "data_extraction/ultimate_extractor.nu"
              # "kubernetes/mod.nu"
              "nix/nix.nu"
            ]
          )
        )
      }

      use ${nu-scripts}/custom-completions/yarn/yarn-v4-completions.nu
    '';
  };
  xdg.configFile."nushell/starship.nu".source = ./starship.nu;
  xdg.configFile."nushell/home.nu".source = pkgs.writeText "home.nu" ''
    ${
      lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: "$env.${name} = \"${value}\"") home.sessionVariables)
    }
  '';
}
