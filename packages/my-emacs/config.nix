{
  runCommand,
  emacs,
}:
runCommand "default.el" {} ''
  cp ${./emacs.org} $TMPDIR/emacs.org
  cd $TMPDIR
  ${emacs}/bin/emacs --batch -Q \
                     -l org emacs.org \
                     -f org-babel-tangle
  mv emacs.el $out
''
