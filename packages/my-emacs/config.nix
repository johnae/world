{
  runCommand,
  emacs,
  generalGlobalPrefix ? "C-SPC",
}:
runCommand "emacs-config" {} ''
  mkdir -p $out
  cp ${./emacs.org} $out/emacs.org
  substituteInPlace $out/emacs.org \
    --replace ':global-prefix "C-SPC"' ':global-prefix "${generalGlobalPrefix}"'
  cd $out
  ${emacs}/bin/emacs --batch -Q \
                     -l org emacs.org \
                     -f org-babel-tangle
''
