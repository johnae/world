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
    settings = {
      user = {
        name = userinfo.fullName;
        email = userinfo.email;
      };
      ui = {
        editor = "hx";
      };
    };
  };
}
