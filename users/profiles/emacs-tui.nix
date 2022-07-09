{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    package = pkgs.my-emacs-tui;
  };
}
