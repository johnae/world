{
  config,
  lib,
  ...
}: let
  inherit (config) userinfo;
in {
  programs.git = {
    userName = lib.mkDefault userinfo.fullName;
    userEmail = lib.mkDefault userinfo.email;
    enable = true;
    lfs.enable = true;
    delta = {
      enable = true;
      options.features = "decorations side-by-side line-numbers";
    };
    ignores = [
      "*~"
      ".direnv*"
      ".devenv*"
    ];
    extraConfig = {
      github.user = lib.mkDefault userinfo.githubUser;
      gitlab.user = lib.mkDefault userinfo.gitlabUser;
      core.editor = "hx";
      push.default = "upstream";
      pull.rebase = true;
      rebase.autoStash = true;
      init.defaultBranch = "main";
      color = {
        ui = "auto";
        branch = "auto";
        status = "auto";
        diff = "auto";
        interactive = "auto";
        grep = "auto";
        decorate = "auto";
        showbranch = "auto";
        pager = true;
      };
    };
  };
}
