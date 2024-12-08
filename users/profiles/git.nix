{config, ...}: let
  inherit (config) userinfo;
in {
  programs.git = {
    userName = userinfo.fullName;
    userEmail = userinfo.email;
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
      github.user = userinfo.githubUser;
      gitlab.user = userinfo.gitlabUser;
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
