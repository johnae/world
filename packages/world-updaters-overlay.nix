final: prev: let
  inherit (final) writeStrictShellScriptBin ripgrep buildEnv curl jq;

  update-cargo-vendor-sha = writeStrictShellScriptBin "update-cargo-vendor-sha" ''
    if [ -z "$1" ]; then
        echo "USAGE: $0 <attribute>"
        echo "EXAMPLE: $0 ripgrep"
        exit 1
    fi

    attr="$1"
    path="packages/$attr"/default.nix
    sed -i 's|cargoSha256.*|cargoSha256 = "0000000000000000000000000000000000000000000000000000";|' "$path"

    log="$(mktemp nix-rustbuild-log-"$attr".XXXXXXX)"
    trap 'rm -f $log' EXIT

    nix build .#"$attr" 2>&1 | tee "$log" || true
    cargoSha256="$(grep 'got:.*sha256.*' "$log" | awk '{print $NF}')"
    echo Setting cargoSha256 for "$attr" to "$cargoSha256"
    sed -i "s|cargoSha256.*|cargoSha256 = \"$cargoSha256\";|" "$path"
  '';

  update-all-cargo-vendor-shas = writeStrictShellScriptBin "update-all-cargo-vendor-shas" ''
    for rpkg in $(${ripgrep}/bin/rg -l cargoSha256 ./packages/*/* | awk -F'/' '{print $3}'); do
      ${update-cargo-vendor-sha}/bin/update-cargo-vendor-sha "$rpkg"
    done
  '';

  update-fixed-output-derivation-sha = writeStrictShellScriptBin "update-fixed-output-derivation-sha" ''
    if [ -z "$1" ]; then
        echo "USAGE: $0 <attribute>"
        echo "EXAMPLE: $0 argocd-ui"
        exit 1
    fi

    attr="$1"
    path="packages/$attr"/default.nix
    what=outputHash;
    if grep -q "vendorSha256" "$path"; then
      what="vendorSha256"
    fi

    sed -i "s|$what =.*|$what = \"0000000000000000000000000000000000000000000000000000\";|" "$path"

    log="$(mktemp nix-fixed-output-drv-log-"$attr".XXXXXXX)"
    trap 'rm -f $log' EXIT

    nix build .#"$attr" 2>&1 | tee "$log" || true
    hash="$(grep 'got:.*sha256.*' "$log" | awk '{print $NF}')"
    echo Setting "$what" for "$attr" to "$hash"
    sed -i "s|$what =.*|$what = \"$hash\";|" "$path"
  '';

  update-all-fixed-output-derivation-shas = writeStrictShellScriptBin "update-all-fixed-output-derivation-shas" ''
    for fopkg in $(${ripgrep}/bin/rg -l "outputHash|vendorSha256" ./packages/*/* | awk -F'/' '{print $3}'); do
      ${update-fixed-output-derivation-sha}/bin/update-fixed-output-derivation-sha "$fopkg"
    done
  '';

  update-github-release-flake-inputs = writeStrictShellScriptBin "update-github-release-flake-inputs" ''
    IFS=$'\n'
    TOKEN=''${1:-}
    curlargs=()
    if [ "$TOKEN" != "" ]; then
      curlargs=(-H "Authorization: token $TOKEN")
    fi
    for ghpkg in $(${ripgrep}/bin/rg -N "gh-release-update" ./flake.nix); do
      if echo "$ghpkg" | grep -q "releases"; then
        echo releases
        uri="$(echo "$ghpkg" | awk -F'"' '{print $2}' | awk -F'github.com/' '{print $2}')"
        echo "uri: $uri"
        owner="$(echo "$uri" | awk -F'/' '{print $1}')"
        repo="$(echo "$uri" | awk -F'/' '{print $2}')"
        latest="$(${curl}/bin/curl -H "Accept: application/vnd.github.v3+json" "''${curlargs[@]}" "https://api.github.com/repos/$owner/$repo/releases" | \
          ${jq}/bin/jq '[.[] | select(.draft == false) | select(.prerelease == false)][0].tag_name' -r)"
        echo sed -i -E "s|$owner/$repo/releases/download/[0-9v.]+|$owner/$repo/releases/download/$latest|g" flake.nix
      else
        uri="$(echo "$ghpkg" | awk -F'"' '{print $2}' | awk -F':' '{print $2}')"
        echo "uri: $uri"
        owner="$(echo "$uri" | awk -F'/' '{print $1}')"
        repo="$(echo "$uri" | awk -F'/' '{print $2}')"
        latest="$(${curl}/bin/curl -H "Accept: application/vnd.github.v3+json" "''${curlargs[@]}" "https://api.github.com/repos/$owner/$repo/releases" | \
          ${jq}/bin/jq '[.[] | select(.draft == false) | select(.prerelease == false)][0].tag_name' -r)"
        echo sed -i -E "s|$owner/$repo/[0-9v.]+|$owner/$repo/$latest|g" flake.nix
      fi
    done
  '';
in {
  world-updaters = buildEnv {
    name = "world-updaters";
    paths = [
      update-github-release-flake-inputs
      update-cargo-vendor-sha
      update-all-cargo-vendor-shas
      update-fixed-output-derivation-sha
      update-all-fixed-output-derivation-shas
    ];
  };
}
