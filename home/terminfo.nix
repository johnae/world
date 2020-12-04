{ pkgs, config, lib, options, ... }:
let
  terminfoDefinition = pkgs.writeText "terminfoDefs" ''
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
  terminfo = pkgs.runCommand "terminfo" { } ''
    ${pkgs.ncurses}/bin/tic -x -o $out "${terminfoDefinition}"
  '';
in
{

  home.file.".terminfo".source = terminfo;

}
