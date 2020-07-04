{ fetchgit
, writeText
, isync
, pandoc
, imapnotify
, wl-clipboard
, mu
, runCommand
, ...
}:
let
  emacsConfig = runCommand "emacs.el" {} ''
    cp ${./emacs.el} "$out"
    substituteInPlace "$out" \
                      --subst-var-by MUSE_LOAD_PATH \
                      "${mu}/share/emacs/site-lisp/mu4e" \
                      --subst-var-by MBSYNC \
                      "${isync}/bin/mbsync" \
                      --subst-var-by PANDOC \
                      "${pandoc}/bin/pandoc" \
                      --subst-var-by IMAPNOTIFY \
                      "${imapnotify}/bin/imapnotify" \
                      --subst-var-by WLCOPY \
                      "${wl-clipboard}/bin/wl-copy" \
                      --subst-var-by WLPASTE \
                      "${wl-clipboard}/bin/wl-paste"
  '';
in
{ inherit emacsConfig; }
