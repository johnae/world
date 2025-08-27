{config, ...}: let
  inherit (config) userinfo;
in {
  # home.packages = [pkgs.lazyjj];
  programs.jujutsu = {
    enable = true;
    ediff = false;
    settings = {
      user = {
        inherit (userinfo) email;
        name = userinfo.fullName;
      };
      ui = {
        editor = "emacsclient -a ''";
        pager = "delta";
        diff-formatter = ["difft" "--color=always" "$left" "$right"];
      };
    };
  };
}
