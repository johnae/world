{ pkgs, config, lib, options, ... }:
let
  secretHosts =
    builtins.mapAttrs
      (_: value:
        value // {
          forwardAgent = true;
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
      };
      "rhea" = {
        hostname = "rhea";
        forwardAgent = true;
      };
      "titan" = {
        hostname = "titan";
        forwardAgent = true;
      };
    };
  };
}
