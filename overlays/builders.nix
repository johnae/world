final: prev:

{

  ## Fail on undefined variables etc. and enforce shellcheck on all scripts.
  ## We skip SC1117 see: https://github.com/koalaman/shellcheck/wiki/SC1117 as
  ## it has been retired (and is kind of annoying).
  writeStrictShellScriptBin = name: text:
    prev.writeTextFile {
      inherit name;
      executable = true;
      destination = "/bin/${name}";
      text = ''
        #!${prev.stdenv.shell}
        set -euo pipefail
        ${text}
      '';
      checkPhase = ''
        ## check the syntax
        ${prev.stdenv.shell} -n $out/bin/${name}
        ## shellcheck
        ${prev.shellcheck}/bin/shellcheck -e SC1117 -s bash -f tty $out/bin/${name}
      '';
    };

  writeStrictShellScript = name: text:
    prev.writeTextFile {
      inherit name;
      executable = true;
      text = ''
        #!${prev.stdenv.shell}
        set -euo pipefail
        ${text}
      '';
      checkPhase = ''
        ${prev.stdenv.shell} -n $out
        ${prev.shellcheck}/bin/shellcheck -s bash -f tty $out
      '';
    };


  ## A helper for creating shell script derivations from files
  ## see above - enables one to get syntax highlighting while
  ## developing.
  mkStrictShellScript =
    { name
    , src
    , substitutions ? { }
    }: prev.stdenv.mkDerivation {
      inherit name;
      buildCommand = ''
        install -v -D -m755 ${src} $out/bin/${name}
        ${prev.substituteInPlace "$out/bin/${name}" substitutions}

        if S=$(grep -E '@[a-zA-Z0-9-]+@' < $out/bin/${name}); then
          WHAT=$(echo "$S" | sed 's|.*\(@.*@\).*|\1|g')
          cat<<ERR

          ${name}:
             '$WHAT'
               ^ this doesn't look right, forgotten substitution?

        ERR
          exit 1
        fi

        ## check the syntax
        ${prev.stdenv.shell} -n $out/bin/${name}

        ## shellcheck
        ${prev.shellcheck}/bin/shellcheck -x -e SC1117 -s bash -f tty $out/bin/${name}
      '';
    };
}
