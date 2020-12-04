{ pkgs, config, lib, options, ... }:
let
  secretHosts =
    builtins.mapAttrs
      (_: value:
        value // {
          forwardAgent = true;
          extraOptions = {
            inherit remoteCommand;
            requestTTY = "yes";
          };
        }
      )
      (
        if builtins.getEnv "NIX_TEST" != ""
        then { }
        else
          builtins.exec [
            "${pkgs.sops}/bin/sops"
            "-d"
            "${pkgs.inputs.secrets}/ssh-hosts/hosts.nix"
          ]
      );

  ## runs tmux with theme etc on remote nix machines
  remoteCommand = builtins.replaceStrings [ "\n" ] [ " " ] ''
    nix-shell -E '
      with import <nixpkgs> {};
      let nordTheme = fetchFromGitHub {
                        owner = "arcticicestudio";
                        repo = "nord-tmux";
                        rev = "b0fd5838dbd5f3cf55eefd83ac84f3f9ac076610";
                        sha256 = "14xhh49izvjw4ycwq5gx4if7a0bcnvgsf3irywc3qps6jjcf5ymk";
                      };
          start-tmux = pkgs.writeShellScriptBin "start-tmux" "
            mkdir -p ~/.ssh;
            chmod 0700 ~/.ssh;
            if [ ! -S ~/.ssh/ssh_auth_sock ] && [ -S \"$SSH_AUTH_SOCK\" ]; then
              ln -sf $SSH_AUTH_SOCK ~/.ssh/ssh_auth_sock;
            fi;
            echo \"eval (''${starship}/bin/starship init fish)\" > ~/.config/fish/config.fish;

            SHELL=fish
            PATH=$PATH:''${pkgs.tmux}/bin
            SSH_AUTH_SOCK=~/.ssh/ssh_auth_sock

            exec tmux new -A -s default\\\;
                 set-option -g default-terminal \"xterm-256color\"\\\;
                 set-option -ga terminal-overrides \",*256col*:Tc\"\\\;
                 set-option -sg escape-time 20\\\;
                 set-option -g mode-keys vi\\\;
                 set-option -g mouse on\\\;
                 bind Escape copy-mode\\\;
                 bind -T copy-mode-vi Escape send -X cancel\\\;
                 run-shell ''${nordTheme}/nord.tmux
          ";
      in pkgs.mkShell { buildInputs = [ start-tmux ]; }'
    --run start-tmux
  '';
in
{
  programs.ssh = {
    enable = true;
    forwardAgent = true;
    serverAliveInterval = 60;
    controlMaster = "auto";
    controlPersist = "30m";
    matchBlocks = secretHosts // {
      "*.compute.amazonaws.com" = {
        extraOptions = {
          strictHostKeyChecking = "no";
          userKnownHostsFile = "/dev/null";
        };
      };
      "github github.com" = {
        hostname = "github.com";
        user = "git";
        extraOptions = {
          preferredAuthentications = "publickey";
        };
      };
      "hyperion" = {
        hostname = "hyperion";
        forwardAgent = true;
        extraOptions = {
          inherit remoteCommand;
          requestTTY = "yes";
        };
      };
      "rhea" = {
        hostname = "rhea";
        forwardAgent = true;
        extraOptions = {
          inherit remoteCommand;
          requestTTY = "yes";
        };
      };
      "titan" = {
        hostname = "titan";
        forwardAgent = true;
        extraOptions = {
          inherit remoteCommand;
          requestTTY = "yes";
        };
      };
    };
  };
}
