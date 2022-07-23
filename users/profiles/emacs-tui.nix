{pkgs, ...}: {
  home.file.".emacs".source = "${pkgs.my-emacs-tui-config}/emacs.el";
  programs.emacs = {
    enable = true;
    package = pkgs.my-emacs-tui;
  };
}
