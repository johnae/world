{ pkgs, config, ... }:

let
  extraConfig = config.home.extraConfig;
in
{
  programs.git = {
    userName = extraConfig.userFullName;
    userEmail = extraConfig.userEmail;
    enable = true;
    delta = {
      enable = true;
      options.features = "decorations side-by-side line-numbers";
    };
    extraConfig = {
      github.user = extraConfig.githubUser;
      gitlab.user = extraConfig.gitlabUser;
      core.editor = "${pkgs.my-emacs}/bin/emacsclient -c";
      push.default = "upstream";
      pull.rebase = true;
      rebase.autoStash = true;
      url."git@github.com:".insteadOf = "https://github.com/";
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
