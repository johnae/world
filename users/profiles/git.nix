{ pkgs, config, lib, options, ... }:

{
  programs.git = {
    enable = true;
    delta = {
      enable = true;
      options.features = "decorations side-by-side line-numbers";
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
    };
  };
}
