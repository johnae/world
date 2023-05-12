{
  runCommand,
  emacs,
  loadPaths ? [],
}:
runCommand "default.el" {} ''
  cp ${./emacs.org} $TMPDIR/emacs.org
  cd $TMPDIR
  ${emacs}/bin/emacs --batch -Q \
                     -l org emacs.org \
                     -f org-babel-tangle
  substitute emacs.el $out --replace ";; EXTRA_LOAD_PATHS" \
   "${(builtins.concatStringsSep "\n" (map (p: "(add-to-list 'load-path \\\"${p}\\\")") loadPaths))}"
''
