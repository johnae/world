{
  stdenv,
  lib,
  writeStrictShellScriptBin,
  buildEnv,
  wl-clipboard,
  fire,
  fd,
  rbw,
  rofi-wayland,
  skim,
  pass,
  wpa_supplicant,
  hostname,
}: let
  addToBinPath = pkgs: ''
    export PATH=${lib.makeBinPath pkgs}''${PATH:+:''${PATH}}
  '';

  git-credential-pass = writeStrictShellScriptBin "git-credential-pass" ''
    passfile="$1"
    echo password="$(${pass}/bin/pass show "$passfile" | head -1)"
  '';

  sk-sk = writeStrictShellScriptBin "sk-sk" ''
    ${addToBinPath [skim]}
    SK_MIN_HEIGHT=''${SK_MIN_HEIGHT:-100}
    SK_MARGIN=''${SK_MARGIN:-5,5,5,5}
    SK_PROMPT=''${SK_PROMPT:- >}

    export SKIM_DEFAULT_OPTIONS=''${SK_OPTS:-"--reverse"}
    exec sk --min-height="$SK_MIN_HEIGHT" \
        --margin="$SK_MARGIN" \
        --prompt="$SK_PROMPT"
  '';

  project-select = writeStrictShellScriptBin "project-select" ''
    ${addToBinPath [sk-sk fd]}
    projects=$*
    if [ -z "$projects" ]; then
      echo "Please provide the project root directories to search as arguments"
      exit 1
    fi
    export SK_PROMPT="goto project >"
    export SK_OPTS="--color=bw --tac --reverse"
    # shellcheck disable=SC2086
    fd -d 8 -pHI -t f '.*\.git/config|.*\.projectile' $projects | \
      sed -e 's|/\.git/config||g' \
          -e 's|/\.projectile||g' \
          -e "s|$HOME/||g" | \
      sk-sk | \
      xargs -I{} echo "$HOME/{}"
  '';

  launch = writeStrictShellScriptBin "launch" ''
    cmd=$*
    if [ -z "$cmd" ]; then
      read -r cmd
    fi
    echo "${fire}/bin/fire $cmd" | ${stdenv.shell}
  '';

  rofi-rbw = writeStrictShellScriptBin "rofi-rbw" ''
    ${addToBinPath [rofi-wayland rbw wl-clipboard]}
    passonly=''${passonly:-}
    selection="$(rbw list --fields name,user | \
       sed 's|\t|/|g' | \
       rofi -normal-window -matching fuzzy -i -dmenu)"

    entry="$(echo "$selection" | awk -F'/' '{print $1}')"
    login="$(echo "$selection" | awk -F'/' '{print $2}')"
    pass="$(rbw get "$entry" "$login")"

    if [ -z "$passonly" ]; then
      echo -n "$login" | wl-copy -onf
      echo -n "$pass" | wl-copy -onf
    else
      echo -n "$pass" | wl-copy -onf
    fi
  '';

  spotify-cmd = writeStrictShellScriptBin "spotify-cmd" ''
    echo "$@" > "$XDG_RUNTIME_DIR"/spotnix_input
  '';

  rofi-spotnix-play = writeStrictShellScriptBin "rofi-spotnix-play" ''
    ${addToBinPath [rofi-wayland wl-clipboard]}
    t="$1"
    awk -F ' - spotify:' \
           '{print "<span size=\"medium\">"$1"</span><span size=\"1\" alpha=\"1\">#spotify:"$2"</span>"}' \
           < "$XDG_RUNTIME_DIR"/spotnix_output | \
           rofi -normal-window -matching fuzzy -i -dmenu -markup-rows -format p -p "$t >" | \
           awk -F'#' '{print "play "$2}' > "$XDG_RUNTIME_DIR"/spotnix_input
  '';

  rofi-spotify-search = writeStrictShellScriptBin "rofi-spotify-search" ''
    ${addToBinPath [rofi-wayland wl-clipboard]}
    t="$1"
    search="$(rofi -normal-window -dmenu -p "search $t >")"
    echo search_"$t" "$search" > "$XDG_RUNTIME_DIR"/spotnix_input
    ${rofi-spotnix-play}/bin/rofi-spotnix-play "$t"
  '';

  update-wireguard-keys = writeStrictShellScriptBin "update-wireguard-keys" ''
    ${addToBinPath [hostname pass]}
    IFS=$'\n'
    HN="$(hostname)"
    for KEY in $(find "$PASSWORD_STORE_DIR"/vpn/wireguard/"$HN"/ -type f -print0 | xargs -0 -I{} basename {}); do
      KEYNAME=$(basename "$KEY" .gpg)
      echo "Ensure wireguard key \"$KEYNAME\" is available"
      pass show "vpn/wireguard/$HN/$KEYNAME" | sudo tee /var/lib/wireguard/"$KEYNAME" > /dev/null
      sudo chmod 0600 /var/lib/wireguard/"$KEYNAME"
    done
  '';

  update-wifi-networks = writeStrictShellScriptBin "update-wifi-networks" ''
    ${addToBinPath [pass]}
    IFS=$'\n'
    for NET in $(find "$PASSWORD_STORE_DIR"/wifi/networks/ -type f -print0 | xargs -0 -I{} basename {}); do
      NETNAME=$(basename "$NET" .gpg)
      echo "Ensure wireless network \"$NETNAME\" is available"
      pass show "wifi/networks/$NETNAME" | sudo tee "/var/lib/iwd/$NETNAME.psk" > /dev/null
    done
  '';

  add-wifi-network = writeStrictShellScriptBin "add-wifi-network" ''
    ${addToBinPath [wpa_supplicant pass update-wifi-networks]}
    NET=''${1:-}
    PASS=''${2:-}
    if [ -z "$NET" ]; then
      echo Please provide the network as first argument
      exit 1
    fi
    if [ -z "$PASS" ]; then
      echo Please provide the password as second argument
      exit 1
    fi
    PSK=$(wpa_passphrase "$1" "$2" | grep "[^#]psk=" | awk -F'=' '{print $2}')
    if [ -z "$PSK" ]; then
      echo Hmm PSK was empty
      exit 1
    fi
    cat <<EOF | pass insert -m "wifi/networks/$NET"
    [Security]
    PreSharedKey=$PSK
    Passphrase=$PASS
    EOF
    update-wifi-networks
  '';
in
  buildEnv {
    name = "scripts";
    paths = [
      project-select
      launch
      git-credential-pass
      add-wifi-network
      update-wifi-networks
      update-wireguard-keys
      spotify-cmd
      rofi-rbw
      rofi-spotify-search
    ];
    meta.platforms = lib.platforms.linux;
  }
