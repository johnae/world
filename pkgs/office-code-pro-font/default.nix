{ stdenv, fetchurl }:

stdenv.mkDerivation rec {
  version = "2019-04-23";
  name = "font-office-code-pro-${version}";
  at = "fcf415b0635765a59085f93ee2186a53adfecac4";

  srcs = [
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/johnae/Office-Code-Pro/${at}/Fonts/Office%20Code%20Pro%20D%20Patched/OfficeCodeProD-Bold.otf";
        name = "office-code-pro-d-bold.otf";
        sha256 = "0pc16fyslqcy7p1lm8x244w4v1ivfl6zvskc2gc4q81qrgvls8yh";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/johnae/Office-Code-Pro/${at}/Fonts/Office%20Code%20Pro%20D%20Patched/OfficeCodeProD-BoldItalic.otf";
        name = "office-code-pro-d-bold-italic.otf";
        sha256 = "00afr9ka6z1mgly03y13bzb5adwcxa4zk56c16aqcv4kc7r6w4qg";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/johnae/Office-Code-Pro/${at}/Fonts/Office%20Code%20Pro%20D%20Patched/OfficeCodeProD-Light.otf";
        name = "office-code-pro-d-light.otf";
        sha256 = "06w21sbpfyccv2fd0i004kghvn1y79amy0r0gi4lsjzyvxcsf2wr";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/johnae/Office-Code-Pro/${at}/Fonts/Office%20Code%20Pro%20D%20Patched/OfficeCodeProD-LightItalic.otf";
        name = "office-code-pro-d-light-italic.otf";
        sha256 = "0a6lhcn4cg46qyr7cnx9czfcvczs58salgx58phacgjl7krpalvy";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/johnae/Office-Code-Pro/${at}/Fonts/Office%20Code%20Pro%20D%20Patched/OfficeCodeProD-Medium.otf";
        name = "office-code-pro-d-medium.otf";
        sha256 = "045s156y94gj0gpkwn1rbkqnxz2c6lrk749z7kjj98x0r7q0llwd";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/johnae/Office-Code-Pro/${at}/Fonts/Office%20Code%20Pro%20D%20Patched/OfficeCodeProD-MediumItalic.otf";
        name = "office-code-pro-d-medium-italic.otf";
        sha256 = "1x0sdfxz8v0qfprmjw7794bf4ddl5jzkdfyflq2c3x79ygf8ckzm";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/johnae/Office-Code-Pro/${at}/Fonts/Office%20Code%20Pro%20D%20Patched/OfficeCodeProD-Regular.otf";
        name = "office-code-pro-d-regular.otf";
        sha256 = "1m9c8dj5vj6198amsd161gg8bg8mvg7xd715x92lbk3pkm899nv8";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/johnae/Office-Code-Pro/${at}/Fonts/Office%20Code%20Pro%20D%20Patched/OfficeCodeProD-RegularItalic.otf";
        name = "office-code-pro-d-regular-italic.otf";
        sha256 = "0pq3ykfqckqckii5cy3mwlwa97mzs1bac4lgj6ln0pp5279ckcdx";
      }
    )
  ];

  phases = [ "unpackPhase" "installPhase" ];

  sourceRoot = "./";

  unpackCmd = ''
    otfName=$(basename $(stripHash $curSrc))
    echo "otfname: $otfName"
    cp $curSrc ./$otfName
  '';

  installPhase = ''
    mkdir -p $out/share/fonts/office-code-pro
    cp *.otf $out/share/fonts/office-code-pro
  '';

  outputHashAlgo = "sha256";
  outputHashMode = "recursive";
  outputHash = "0bvk1j094wxsgl01p8843sw4ys2r8rwg300ip4va4vcwbzr13ysr";

  meta = {
    description = "Office Code Pro Font (patched Source Code Pro Font)";
    homepage = "https://github.com/johnae/Office-Code-Pro";
    platforms = stdenv.lib.platforms.all;
    maintainers = [];
  };
}
