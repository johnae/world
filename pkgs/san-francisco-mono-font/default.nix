{ stdenv, fetchurl }:

# https://github.com/ZulwiyozaPutra/SF-Mono-Font

stdenv.mkDerivation rec {
  version = "2018-06-07";
  name = "font-mono-san-francisco-${version}";
  at = "1409ae79074d204c284507fef9e479248d5367c1";

  srcs = [
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/ZulwiyozaPutra/SF-Mono-Font/${at}/SFMono-Bold.otf";
        name = "san-francisco-mono-bold.otf";
        sha256 = "1i1vy8v6klj7albz147yw8dr7i41aymlvql59aq2jvwfbw355gqq";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/ZulwiyozaPutra/SF-Mono-Font/${at}/SFMono-BoldItalic.otf";
        name = "san-francisco-mono-bold-italic.otf";
        sha256 = "14m93v23fl1a0bralkdh3vrrl6wn52sdiba8y3fw4w31i1byknkn";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/ZulwiyozaPutra/SF-Mono-Font/${at}/SFMono-Heavy.otf";
        name = "san-francisco-mono-heavy.otf";
        sha256 = "0yc0hgr2mx98qay6gjx01lxmkr0dzklx3qz0zd2chldq9saia7gx";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/ZulwiyozaPutra/SF-Mono-Font/${at}/SFMono-HeavyItalic.otf";
        name = "san-francisco-mono-heavy-italic.otf";
        sha256 = "1ijd2w6279vsfzb61jdjz0pvlmyxmp3qd7qq8550dawgbbf4kr3f";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/ZulwiyozaPutra/SF-Mono-Font/${at}/SFMono-Light.otf";
        name = "san-francisco-mono-light.otf";
        sha256 = "06ids347299j67smhcmvy28iyzkh8wymf14zmfyz2wkv8v2xaxy7";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/ZulwiyozaPutra/SF-Mono-Font/${at}/SFMono-LightItalic.otf";
        name = "san-francisco-mono-light-italic.otf";
        sha256 = "1lmdphb345zg8z64v1v09mbqqgxmyp4q4vcxij069gnbgvzcf6il";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/ZulwiyozaPutra/SF-Mono-Font/${at}/SFMono-Medium.otf";
        name = "san-francisco-mono-medium.otf";
        sha256 = "1mqfqgnkas9q65d4zh1353b58v6jrklkbrz25njgz1zkz8p6684x";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/ZulwiyozaPutra/SF-Mono-Font/${at}/SFMono-MediumItalic.otf";
        name = "san-francisco-mono-medium-italic.otf";
        sha256 = "0kq5873bc2vliky9818ahp2chb4n6z824hhqhsqhkn0w3866ngzf";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/ZulwiyozaPutra/SF-Mono-Font/${at}/SFMono-Regular.otf";
        name = "san-francisco-mono-regular.otf";
        sha256 = "13mhkvprr9sg1hd433wrdn88l53n9fh4vzl5m5ja7d0bxrlprrj1";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/ZulwiyozaPutra/SF-Mono-Font/${at}/SFMono-RegularItalic.otf";
        name = "san-francisco-mono-regular-italic.otf";
        sha256 = "0xv7zvm64znj2afw6qmrbiiya2rvd9ixjzpbvxng9iia23aagwvk";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/ZulwiyozaPutra/SF-Mono-Font/${at}/SFMono-Semibold.otf";
        name = "san-francisco-mono-semibold.otf";
        sha256 = "0ap933z8z4fjmh7dwyhangfxhw5kixdrcwmg7qfkkrzdmrah5w3l";
      }
    )
    (
      fetchurl {
        url =
          "https://raw.githubusercontent.com/ZulwiyozaPutra/SF-Mono-Font/${at}/SFMono-SemiboldItalic.otf";
        name = "san-francisco-mono-semibold-italic.otf";
        sha256 = "06a1gwri8pw1kzqbcdq20f8xyiyslk6jvfiwqncyapqv6admaki1";
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
    mkdir -p $out/share/fonts/san-francisco-mono
    cp *.otf $out/share/fonts/san-francisco-mono
  '';

  outputHashAlgo = "sha256";
  outputHashMode = "recursive";
  outputHash = "0gknif6bhcw3fgvccv2ngh4dn782ddl9mlnk5a6f8w5bj5gnghba";

  meta = {
    description = "San Francisco Mono Font by Apple";
    homepage = "https://github.com/ZulwiyozaPutra/SF-Mono-Font";
    license = stdenv.lib.licenses.asl20;
    platforms = stdenv.lib.platforms.all;
    maintainers = [];
  };
}
