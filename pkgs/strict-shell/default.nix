{ stdenv, shellcheck, lib, writeTextFile, ... }:
let
  inherit (lib) concatStringsSep mapAttrsToList;
  setToStringSep = sep: x: fun: concatStringsSep sep (mapAttrsToList fun x);

  substituteInPlace = file: substitutions: ''
    substituteInPlace ${file} \
    ${
      setToStringSep " " substitutions
        (name: value: ''--subst-var-by ${name} "${value}"'')
    }
  '';


  writeStrictShellScriptBin = name: text:
    writeTextFile {
      inherit name;
      executable = true;
      destination = "/bin/${name}";
      text = ''
        #!${stdenv.shell}
        set -euo pipefail
        ${text}
      '';
      checkPhase = ''
        ${stdenv.shell} -n $out/bin/${name}
        ${shellcheck}/bin/shellcheck -s bash -f tty $out/bin/${name}
      '';
    };

  mkStrictShellScript = { name, src, substitutions ? { } }:
    stdenv.mkDerivation {
      inherit name;
      buildCommand = ''
        install -v -D -m755 ${src} $out/bin/${name}
        ${substituteInPlace "$out/bin/${name}" substitutions}

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
        ${stdenv.shell} -n $out/bin/${name}

        ## shellcheck
        ${shellcheck}/bin/shellcheck -x -e SC1117 -s bash -f tty $out/bin/${name}
      '';
    };

  writeStrictShellScript = name: text:
    writeTextFile {
      inherit name;
      executable = true;
      text = ''
        #!${stdenv.shell}
        set -euo pipefail
        ${text}
      '';
      checkPhase = ''
        ${stdenv.shell} -n $out
        ${shellcheck}/bin/shellcheck -s bash -f tty $out
      '';
    };


  ## Takes shell code on stdin, runs shellcheck on it and automatically adds
  ## the inofficial strict-mode - eg. "set -euo pipefail"
  strict-bash = writeStrictShellScriptBin "strict-bash" ''
    ## first define a random script name and make it executable
    script="$(mktemp /tmp/script.XXXXXX.sh)"
    chmod +x "$script"

    ## then add a prelude (eg. shebang + "strict mode")
    cat<<EOF>"$script"
    #!${stdenv.shell}
    set -euo pipefail

    EOF

    ## now send stdin to the above file - which
    ## follows the defined prelude
    cat>>"$script"

    ## do a syntax check
    #!${stdenv.shell} -n "$script"

    ## check the script for common errors
    ${shellcheck}/bin/shellcheck -e SC1117 -s bash -f tty "$script"

    ## if all of the above went well - execute the script
    "$script"
  '';
in
{
  inherit writeStrictShellScript writeStrictShellScriptBin
    mkStrictShellScript strict-bash;
}
