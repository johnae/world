{ fetchgit
, writeText
, pandoc
, imapnotify
, wl-clipboard
, notmuch
, runCommand
, pass
, ...
}:
let
  emacsConfig = runCommand "emacs.el" { } ''
    cp ${./emacs.el} "$out"
    substituteInPlace "$out" \
                      --subst-var-by NOTMUCH_LOAD_PATH \
                      "${notmuch.emacs}/share/emacs/site-lisp" \
                      --subst-var-by PANDOC \
                      "${pandoc}/bin/pandoc" \
                      --subst-var-by PASS \
                      "${pass}/bin/pass" \
                      --subst-var-by WLCOPY \
                      "${wl-clipboard}/bin/wl-copy" \
                      --subst-var-by WLPASTE \
                      "${wl-clipboard}/bin/wl-paste"
  '';
in
{ inherit emacsConfig; }
