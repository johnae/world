{
  config,
  lib,
  ...
}: let
  inherit (config) userinfo;
in {
  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options.features = "decorations side-by-side line-numbers";
  };
  programs.git = {
    settings = {
      user = {
        name = lib.mkDefault userinfo.fullName;
        email = lib.mkDefault userinfo.email;
      };
      github.user = lib.mkDefault userinfo.githubUser;
      gitlab.user = lib.mkDefault userinfo.gitlabUser;
      core.editor = "emacsclient -a''";
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
    enable = true;
    lfs.enable = true;
    ignores = [
      "*~"
      ".direnv*"
      ".devenv*"
    ];
    signing.format = "ssh";
  };
}
