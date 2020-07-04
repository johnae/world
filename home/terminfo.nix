{ pkgs, config, lib, options }:

{

  xdg.configFile."terminfo/xterm-24bits.conf".text = ''
    # Use colon separators.
    xterm-24bit|xterm with 24-bit direct color mode,
      use=xterm-256color,
      setb24=\E[48:2:%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%dm,
      setf24=\E[38:2:%p1%{65536}%/%d:%p1%{256}%/%{255}%&%d:%p1%{255}%&%dm,
    # Use semicolon separators.
    xterm-24bits|xterm with 24-bit direct color mode,
      use=xterm-256color,
      setb24=\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
      setf24=\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
  '';

  xdg.configFile."terminfo/xterm-24bits.conf".onChange = ''
    for file in ~/.config/terminfo/*; do
      ${pkgs.ncurses}/bin/tic -x -o ~/.terminfo "$file"
    done
  '';

}
