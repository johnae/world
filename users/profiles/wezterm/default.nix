{config, ...}: {
  programs.wezterm = {
    enable = true;
    extraConfig = builtins.replaceStrings ["<devRemote>"] [config.userinfo.devRemote] (builtins.readFile ./wezterm.lua);
  };
}
