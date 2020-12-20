{ pkgs, config, lib, options, ... }:

{
  programs.git = {
    enable = true;
    delta = {
      enable = true;
      options.features = "decorations side-by-side line-numbers";
    };
    userName = "John Axel Eriksson";
    userEmail = "john@insane.se";
    signing = {
      key = "0x04ED6F42C62F42E9";
      signByDefault = true;
    };
    extraConfig = {
      core.editor = "${pkgs.my-emacs}/bin/emacsclient -c";
      push.default = "upstream";
      pull.rebase = true;
      rebase.autoStash = true;
      url."git@github.com:".insteadOf = "https://github.com/";
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
      credential = {
        "https://github.com" = {
          username = "johnae";
          helper = "pass web/github.com/johnae";
        };
        "https://repo.insane.se" = {
          username = "johnae";
          helper = "pass web/repo.insane.se/johnae";
        };
      };
    };
  };
}
