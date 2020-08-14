{ config, stdenv, lib, callPackage, fetchurl, nss_3_44 }:
let
  common = opts: callPackage (import ./common.nix opts) { };
  ffversion = "79.0";
in
common {
  pname = "firefox";
  inherit ffversion;
  src = fetchurl {
    url = "mirror://mozilla/firefox/releases/${ffversion}/source/firefox-${ffversion}.source.tar.xz";
    sha512 = "0zgf7wdcz992a4dy1rj0ax0k65an7h9p9iihka3jy4jd7w4g2d0x4mxz5iqn2y26hmgnkvjb921zh28biikahgygqja3z2pcx26ic0r";
  };

  patches = [
    ./no-buildconfig-ffx76.patch
  ];

  meta = {
    description = "A web browser built from Firefox source tree";
    homepage = "http://www.mozilla.com/en-US/firefox/";
    maintainers = [
      {
        email = "john@insane.se";
        github = "johnae";
        name = "John Axel Eriksson";
      }
    ];
    platforms = lib.platforms.unix;
    badPlatforms = lib.platforms.darwin;
    broken = stdenv.buildPlatform.is32bit; # since Firefox 60, build on 32-bit platforms fails with "out of memory".
    # not in `badPlatforms` because cross-compilation on 64-bit machine might work.
    license = lib.licenses.mpl20;
  };
}
