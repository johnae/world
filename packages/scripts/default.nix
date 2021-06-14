{ stdenv
, lib
, writeScriptBin
, writeStrictShellScriptBin
, buildEnv
, wl-clipboard
, bashInteractive
, fire
, fd
, rbw
, wofi
, skim
, pass
, wpa_supplicant
, gnupg
, alacritty
, libnotify
, hostname
, procps
}:
let

  addToBinPath = pkgs: ''
    export PATH=${lib.makeBinPath pkgs}''${PATH:+:''${PATH}}
  '';

  git-credential-pass = writeStrictShellScriptBin "git-credential-pass" ''
    passfile="$1"
    echo password="$(${pass}/bin/pass show "$passfile" | head -1)"
  '';

  sk-sk = writeStrictShellScriptBin "sk-sk" ''
    ${addToBinPath [ skim ]}
    SK_MIN_HEIGHT=''${SK_MIN_HEIGHT:-100}
    SK_MARGIN=''${SK_MARGIN:-5,5,5,5}
    SK_PROMPT=''${SK_PROMPT:- >}

    export SKIM_DEFAULT_OPTIONS=''${SK_OPTS:-"--reverse"}
    exec sk --min-height="$SK_MIN_HEIGHT" \
        --margin="$SK_MARGIN" \
        --prompt="$SK_PROMPT"
  '';

  project-select = writeStrictShellScriptBin "project-select" ''
    ${addToBinPath [ sk-sk fd ]}
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

  spotify-cmd = writeStrictShellScriptBin "spotify-cmd" ''
    echo "$@" > "$XDG_RUNTIME_DIR"/spotnix_input
  '';

  spotify-play = writeStrictShellScriptBin "spotify-play" ''
    ${addToBinPath [ sk-sk fd spotify-cmd ]}
    export SK_OPTS="--no-bold --color=bw  --height=40 --reverse --no-hscroll --no-mouse"
    TYPE="$1"
    set +e
    search="$(SK_OPTS="$SK_OPTS --print-query" sk-sk < /dev/null)"
    set -e
    echo "$TYPE" "$search" > "$XDG_RUNTIME_DIR"/spotnix_input
    sk-sk < "$XDG_RUNTIME_DIR"/spotnix_output | \
        awk '{print $NF}' | xargs -r -I{} spotify-cmd play {}
  '';

  spotify-play-track = writeStrictShellScriptBin "spotify-play-track" ''
    export SK_PROMPT="find music by track >> "
    exec ${spotify-play}/bin/spotify-play s
  '';

  spotify-play-artist = writeStrictShellScriptBin "spotify-play-artist" ''
    export SK_PROMPT="find music by artist >> "
    exec ${spotify-play}/bin/spotify-play sar
  '';

  spotify-play-album = writeStrictShellScriptBin "spotify-play-album" ''
    export SK_PROMPT="find music by album >> "
    exec ${spotify-play}/bin/spotify-play sab
  '';

  spotify-play-playlist = writeStrictShellScriptBin "spotify-play-playlist" ''
    export SK_PROMPT="find music by playlist >> "
    exec ${spotify-play}/bin/spotify-play sap
  '';

  launch = writeStrictShellScriptBin "launch" ''
    cmd=$*
    if [ -z "$cmd" ]; then
      read -r cmd
    fi
    echo "${fire}/bin/fire $cmd" | ${stdenv.shell}
  '';

  wofi-rbw = writeStrictShellScriptBin "wofi-rbw" ''
    ${addToBinPath [ wofi rbw wl-clipboard ]}
    passonly=''${passonly:-}
    selection="$(rbw list --fields name,user | \
       sed 's|\t|/|g' | \
       wofi --show dmenu --insensitive --lines 10)"

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

  ## must be bashInteractive (not stdenv.shell) for compgen to work
  sk-run = writeScriptBin "sk-run" ''
    #!${bashInteractive}/bin/bash
    ${addToBinPath [ sk-sk launch ]}
    export SK_PROMPT="run >> "
    export SK_OPTS="$SK_OPTS''${SK_OPTS:+ }--no-bold --color=bw --height=40 --no-hscroll --no-mouse --print-query --reverse"

    compgen -c | \
    sort -u | \
    awk '{ if (length($0) > 2) print }' | \
    grep -v -E '^\..*' | \
    sk-sk | \
    tail -n1 | \
    xargs -r launch
  '';

  sk-window = writeStrictShellScriptBin "sk-window" ''
    ${addToBinPath [ procps alacritty ]}
    cmd=''${1:-}
    if [ -z "$cmd" ]; then
      echo "Please provide a command to run in the window as the argument"
      exit 1
    fi
    shift
    pkill -f '\--class sk-window' || true
    # shellcheck disable=SC2086
    exec alacritty --config-file ~/.config/alacritty/alacritty-launcher.yml --class "sk-window" -e $cmd
  '';

  sk-passmenu = writeStrictShellScriptBin "sk-passmenu" ''
    ${addToBinPath [ fd sk-sk launch gnupg libnotify wl-clipboard ]}
    export SK_PROMPT="copy password >> "
    export SK_OPTS="--no-bold --color=bw  --height=40 --reverse --no-hscroll --no-mouse"

    passfile=''${1:-}
    nosubmit=''${nosubmit:-}
    passonly=''${passonly:-}
    _passmenu_didsearch=''${_passmenu_didsearch:-}
    prefix=$(readlink -f "$PASSWORD_STORE_DIR")
    if [ -z "$_passmenu_didsearch" ]; then
      export _passmenu_didsearch=y
      fd --type f -E '/notes/' '.gpg$' "$PASSWORD_STORE_DIR" | \
         sed "s|$prefix/||g" | sed 's|.gpg$||g' | \
         sk-sk | \
         xargs -r -I{} echo "$0 {}" | \
         launch
    fi

    if [ "$passfile" = "" ]; then
      exit
    fi

    error_icon=~/Pictures/icons/essential/error.svg

    getlogin() {
      echo -n "$(basename "$1")"
    }

    getpass() {
      echo -n "$(gpg --decrypt "$prefix/$1.gpg" \
                            2>/dev/null | head -1)"
    }

    login=$(getlogin "$passfile")
    pass=$(getpass "$passfile")

    if [ "$pass" = "" ]; then
      notify-send -i $error_icon -a "Password store" -u critical \
      "Decrypt error" "Error decrypting password file, is your gpg card inserted?"
    else
      if [ -z "$passonly" ]; then
        echo -n "$login" | wl-copy -onf
        echo -n "$pass" | wl-copy -onf
      else
        echo -n "$pass" | wl-copy -onf
      fi
    fi

  '';

  update-wireguard-keys = writeStrictShellScriptBin "update-wireguard-keys" ''
    ${addToBinPath [ hostname pass ]}
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
    ${addToBinPath [ pass ]}
    IFS=$'\n'
    for NET in $(find "$PASSWORD_STORE_DIR"/wifi/networks/ -type f -print0 | xargs -0 -I{} basename {}); do
      NETNAME=$(basename "$NET" .gpg)
      echo "Ensure wireless network \"$NETNAME\" is available"
      pass show "wifi/networks/$NETNAME" | sudo tee "/var/lib/iwd/$NETNAME.psk" > /dev/null
    done
  '';

  add-wifi-network = writeStrictShellScriptBin "add-wifi-network" ''
    ${addToBinPath [ wpa_supplicant pass update-wifi-networks ]}
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
    project-select launch git-credential-pass
    sk-sk sk-run sk-window sk-passmenu
    add-wifi-network update-wifi-networks
    update-wireguard-keys spotify-play-album spotify-play-track
    spotify-cmd spotify-play-artist spotify-play-playlist wofi-rbw
  ];
}
