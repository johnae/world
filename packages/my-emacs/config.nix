{ runCommand, emacs }:

let

  conf = runCommand "emacs-config" { } ''
    mkdir -p $out
    cp ${./emacs.org} $out/emacs.org
    cd $out
    ${emacs}/bin/emacs --batch -Q \
                       -l org emacs.org \
                       -f org-babel-tangle
  '';
in

{
  emacsConfig = "${conf}/emacs.el";
}
