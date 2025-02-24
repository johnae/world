{
  programs.ssh = {
    enable = true;
    forwardAgent = true;
    serverAliveInterval = 60;
    controlMaster = "auto";
    controlPersist = "30m";
    matchBlocks = {
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
