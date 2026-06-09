{config, ...}: let
  inherit (config.home) homeDirectory;
in {
  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    extraConfig = ''
      Include ${homeDirectory}/.ssh/config.d/*.conf
    '';
    settings = {
      "*" = {
        ControlPersist = "30m";
        ControlMaster = "auto";
        ControlPath = "~/.ssh/master-%r@%n:%p";
        ServerAliveInterval = 60;
        ServerAliveCountMax = 3;
        HashKnownHosts = false;
        ForwardAgent = true;
        AddKeysToAgent = "no";
        Compression = false;
      };
      "*.compute.amazonaws.com" = {
        ForwardAgent = false;
        StrictHostKeyChecking = "no";
        UserKnownHostsFile = "/dev/null";
      };
      "github github.com" = {
        HostName = "github.com";
        User = "git";
        ForwardAgent = false;
        PreferredAuthentications = "publickey";
        ControlMaster = "no";
        ControlPath = "none";
      };
    };
  };
}
