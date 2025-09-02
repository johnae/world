{config, ...}: let
  inherit (config.home) homeDirectory;
in {
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    extraConfig = ''
      Include ${homeDirectory}/.ssh/config.d/*.conf
    '';
    matchBlocks = {
      "*" = {
        controlPersist = "30m";
        controlMaster = "auto";
        controlPath = "~/.ssh/master-%r@%n:%p";
        serverAliveInterval = 60;
        serverAliveCountMax = 3;
        hashKnownHosts = false;
        forwardAgent = true;
        addKeysToAgent = "no";
        compression = false;
      };
      "git git.9000.dev" = {
        hostname = "gitssh.9000.dev";
        user = "forgejo";
        port = 2222;
      };
      "*.compute.amazonaws.com" = {
        forwardAgent = false;
        extraOptions = {
          strictHostKeyChecking = "no";
          userKnownHostsFile = "/dev/null";
        };
      };
      "github github.com" = {
        hostname = "github.com";
        user = "git";
        forwardAgent = false;
        extraOptions = {
          preferredAuthentications = "publickey";
          controlMaster = "no";
          controlPath = "none";
        };
      };
    };
  };
}
