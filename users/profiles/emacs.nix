{pkgs, ...}: {
  home.file.".emacs".source = pkgs.my-emacs-config;
  services.emacs = {
    enable = true;
    package = pkgs.my-emacs;
    socketActivation.enable = true;
  };
  programs.emacs = {
    enable = true;
    package = pkgs.my-emacs;
  };
  systemd.user.services.emacs.Service = {
    Environment = [
      ''COLORTERM="truecolor"''
    ];
    LimitNOFILE = 16384;
  };
}
