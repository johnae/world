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
  programs.direnv.enableNushellIntegration = false;
  programs.atuin.enableNushellIntegration = false;
  programs.nushell = {
    enable = true;
    package = pkgs.nushell;
    configFile.source = ./config.nu;
    envFile.source = ./env.nu;
    extraConfig = ''
      source ~/.config/nushell/home.nu
      source ~/.config/nushell/starship.nu
      source ~/.config/nushell/atuin.nu

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
        $env.config.hooks.pre_prompt | append (source ${nu-scripts}/nu-hooks/direnv/config.nu)
      )

      ${
        lib.concatStringsSep "\n"
        (
          map (completion: "use ${nu-scripts}/custom-completions/${completion}/${completion}-completions.nu") [
            "cargo"
            "git"
            "just"
            "nix"
            "npm"
            "typst"
            "man"
            "make"
            "zellij"
          ]
        )
      }

      use ${nu-scripts}/custom-completions/yarn/yarn-v4-completions.nu
    '';
  };
  xdg.configFile."nushell/atuin.nu".source = pkgs.runCommand "atuin.nu" {} ''
    ${pkgs.atuin}/bin/atuin init nu > $out
  '';
  xdg.configFile."nushell/starship.nu".source = ./starship.nu;
  xdg.configFile."nushell/home.nu".source = pkgs.writeText "home.nu" ''
    ${
      lib.concatStringsSep "\n" (lib.mapAttrsToList (name: value: "$env.${name} = \"${value}\"") home.sessionVariables)
    }
  '';
}
