{
  config,
  pkgs,
  ...
}: let
  inherit (config) userinfo;
in {
  home.packages = [pkgs.lazyjj];
  programs.jujutsu = {
    enable = true;
    ediff = false;
    package = pkgs.jujutsu-latest;
    settings = {
      user = {
        inherit (userinfo) email;
        name = userinfo.fullName;
      };
      ui = {
        editor = "hx";
        pager = "delta";
        diff.format = "git";
      };
    };
  };
}
