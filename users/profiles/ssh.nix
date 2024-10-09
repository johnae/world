{
  programs.ssh = {
    enable = true;
    forwardAgent = true;
    serverAliveInterval = 60;
    controlMaster = "auto";
    controlPersist = "30m";
    matchBlocks = {
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
