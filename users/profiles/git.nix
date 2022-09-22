{
  pkgs,
  config,
  lib,
  ...
}: let
  inherit (config) userinfo;
in {
  programs.git = {
    userName = userinfo.fullName;
    userEmail = userinfo.email;
    enable = true;
    delta = {
      enable = true;
      options.features = "decorations side-by-side line-numbers";
    };
    includes =
      lib.mapAttrsToList (dir: email: {
        condition = "gitdir:${dir}";
        contents.user = {inherit email;};
      })
      userinfo.gitIdMap;
    extraConfig = {
      github.user = userinfo.githubUser;
      gitlab.user = userinfo.gitlabUser;
      core.editor = "emacsclient -c";
      push.default = "upstream";
      pull.rebase = true;
      rebase.autoStash = true;
      init.defaultBranch = "main";
      gpg.program = "${pkgs.bpb}/bin/bpb";
      commit.gpgSign = true;
      tag.forceSignAnnotated = true;
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
