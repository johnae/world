{ stdenv, shellcheck, lib, writeTextFile, ... }:
let
  setToStringSep = with lib; sep: x: fun: concatStringsSep sep (mapAttrsToList fun x);

  substituteInPlace = file: substitutions: ''
    substituteInPlace ${file} \
      ${
  setToStringSep " " substitutions
    (name: value: ''--subst-var-by ${name} "${value}"'')
  }
  '';
in
{
  mkStrictShellScript = { name, src, substitutions ? {} }:
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
}
