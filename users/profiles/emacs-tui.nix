{pkgs, ...}: {
  services.emacs = {
    enable = true;
    package = pkgs.my-emacs-tui;
    socketActivation.enable = true;
  };
}
