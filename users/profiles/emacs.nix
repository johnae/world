{pkgs, ...}: {
  services.emacs = {
    enable = true;
    package = pkgs.my-emacs;
    socketActivation.enable = true;
  };
  programs.emacs = {
    enable = true;
    package = pkgs.my-emacs;
  };
}
