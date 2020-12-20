{ pkgs, config, lib, options, ... }:
let
  commandNotFound = pkgs.writeShellScriptBin "command-not-found" ''
    # shellcheck disable=SC1091
    source ${pkgs.nix-index}/etc/profile.d/command-not-found.sh
    command_not_found_handle "$@"
  '';
  ## used to start "something" within a different network namespace
  ## I use it to start my compositor and other stuff within a namespace
  ## with only wireguard interface(s)
  withinNetNS = executable: { netns ? "private" }:
    lib.concatStringsSep " " [
      "${pkgs.dbus}/bin/dbus-run-session" ## sway is actually wrapped and does this, but fish doesn't for example. No harm doing it even for sway.
      "${pkgs.netns-dbus-proxy}/bin/netns-dbus-proxy"
      "netns-exec"
      netns
      executable
    ];

  sway = pkgs.callPackage (pkgs.path + "/pkgs/applications/window-managers/sway/wrapper.nix") {
    extraSessionCommands = ''
      export GDK_BACKEND=wayland
      export MOZ_ENABLE_WAYLAND=1
      export MOZ_USE_XINPUT2=1
      export XCURSOR_THEME=default
      export QT_STYLE_OVERRIDE=gtk
      export _JAVA_AWT_WM_NONREPARENTING=1
      export XDG_CURRENT_DESKTOP=sway
      export XDG_SESSION_TYPE=wayland
      export XDG_SESSION_DESKTOP=sway
      export WLR_DRM_NO_MODIFIERS=1
      if [ ! -e "$HOME"/Pictures/wallpaper.jpg ]; then
        ln "$HOME"/Pictures/default-background.jpg "$HOME"/Pictures/wallpaper.jpg
      fi
    '';
  };

  privateSway = withinNetNS "${sway}/bin/sway" { };
  privateFish = withinNetNS "${pkgs.fish}/bin/fish" { };

  genLaunchOptions = optionList:
    lib.concatStringsSep "\\n" (lib.flatten (
      map
        (
          lib.mapAttrsToList (k: v: "${k}\\texec ${v}")
        )
        optionList
    ));

  genLauncher = optionList: ''
    clear
    set RUN (echo -e "${genLaunchOptions optionList}" | \
        ${pkgs.skim}/bin/sk -p "start >> " --inline-info --margin 40%,40% \
                            --color=bw --height=40 --no-hscroll --no-mouse \
                            --reverse --delimiter='\t' --with-nth 1 | \
                                 ${pkgs.gawk}/bin/awk -F'\t' '{print $2}')
    eval "$RUN"
  '';

  swayDrmDebug = pkgs.writeStrictShellScriptBin "sway-drm-debug" ''
    echo 0xFE | sudo tee /sys/module/drm/parameters/debug # Enable verbose DRM logging
    sudo dmesg -C
    dmesg -w >dmesg.log & # Continuously write DRM logs to a file
    sway -d >sway.log 2>&1 # Reproduce the bug, then exit sway
    fg # Kill dmesg with Ctrl+C
    echo 0x00 | sudo tee /sys/module/drm/parameters/debug
  '';

  drmDebugLaunch = pkgs.writeStrictShellScriptBin "drm-debug-launch" ''
    ln -s ${swayDrmDebug}/bin/sway-drm-debug ~/sway-drm-debug
    echo Please execute ~/sway-drm-debug
    ${pkgs.fish}/bin/fish
  '';

  launcher = genLauncher [
    { "sway" = "${sway}/bin/sway"; }
    { "sway private" = privateSway; }
    { "fish" = "${pkgs.dbus}/bin/dbus-run-session ${pkgs.fish}/bin/fish"; }
    { "fish private" = privateFish; }
    { "sway debug" = "${sway}/bin/sway -d 2> ~/sway.log"; }
    { "sway drm debug" = "${drmDebugLaunch}/bin/drm-debug-launch"; }
  ];

  emacs_tui_no_server = "emacs -nw";
  emacsclient = "emacsclient -c -n -a=";
  emacsclient_tui = "emacsclient -t -a=";

in
{

  xdg.configFile."fish/functions/gcloud_sdk_argcomplete.fish".source = "${pkgs.inputs.google-cloud-sdk-fish-completion}/functions/gcloud_sdk_argcomplete.fish";
  xdg.configFile."fish/completions/gcloud.fish".source = "${pkgs.inputs.google-cloud-sdk-fish-completion}/completions/gcloud.fish";
  xdg.configFile."fish/completions/gsutil.fish".source = "${pkgs.inputs.google-cloud-sdk-fish-completion}/completions/gsutil.fish";
  xdg.configFile."fish/completions/kubectl.fish".source = "${pkgs.inputs.fish-kubectl-completions}/completions/kubectl.fish";

  programs.fish = {
    enable = true;
    shellAbbrs = {
      cat = "bat";
      g = "git";
      et = emacsclient_tui;
      e = emacsclient;
      em = emacs_tui_no_server;
    };
    shellAliases = {
      k8s-run = "${pkgs.kubectl}/bin/kubectl run tmp-shell --generator=run-pod/v1 --rm -i --tty --image=nixpkgs/nix-unstable --restart=Never --attach -- nix-shell -p bashInteractive --run bash";
      ssh = "env TERM=xterm-256color ssh";
    };
    shellInit = ''
      source ${pkgs.skim}/share/skim/key-bindings.fish
      set fish_greeting
      fish_vi_key_bindings ^ /dev/null

      function __fish_command_not_found_handler --on-event fish_command_not_found
        ${commandNotFound}/bin/command-not-found $argv
      end

      function fish_user_key_bindings
        skim_key_bindings

        function skim-jump-to-project-widget -d "Show list of projects"
          begin
            set -lx SK_OPTS "--color=bw --no-hscroll --height=40"
            set -lx dir (${pkgs.project-select}/bin/project-select ~/Development ~/.config)
            if [ "$dir" != "" ]
              cd $dir
              set -lx file (${pkgs.fd}/bin/fd -H -E "\.git" . | "${pkgs.skim}"/bin/sk --color=bw)
              if [ "$file" != "" ]
                ${emacsclient} "$file"
              end
            end
          end
          commandline -f repaint
        end
        bind \cg skim-jump-to-project-widget

        if bind -M insert > /dev/null 2>&1
          bind -M insert \cg skim-jump-to-project-widget
        end

        function skim-jump-to-file-widget -d "Show list of file to open in editor"
          begin
            set -lx SK_OPTS "--color=bw --no-hscroll --height=40"
            set -lx file (${pkgs.fd}/bin/fd -H -E "\.git" . | "${pkgs.skim}"/bin/sk)
            if [ "$file" != "" ]
              ${emacsclient} "$file"
            end
          end
          commandline -f repaint
        end
        bind \cf skim-jump-to-project-widget

        if bind -M insert > /dev/null 2>&1
          bind -M insert \cf skim-jump-to-file-widget
        end

        function kubectx-select -d "Select kubernetes cluster"
          ${pkgs.kubectx}/bin/kubectx
        end
        bind \ck kubectx-select

        if bind -M insert > /dev/null 2>&1
          bind -M insert \ck kubectx-select
        end

        function kubens-select -d "Select kubernetes namespace"
          ${pkgs.kubectx}/bin/kubens
        end
        bind \cn kubectx-select

        if bind -M insert > /dev/null 2>&1
          bind -M insert \cn kubens-select
        end

        function kube-pod-exec -d "Exec into pod"
          begin
            set -lx SK_PROMPT "exec into pod > "
            set -lx SK_OPTS "--color=bw --no-hscroll --height=40"
            set -lx pod (${pkgs.kubectl}/bin/kubectl get pods --no-headers -o custom-columns=name:'{.metadata.name}' | "${pkgs.sk-sk}"/bin/sk-sk)
            if [ "$pod" != "" ]
              set -lx SK_PROMPT "select container > "
              set -lx container (${pkgs.kubectl}/bin/kubectl get pods --no-headers "$pod" -o custom-columns=name:'{.spec.containers[*].name}' | tr ',' '\n' | "${pkgs.sk-sk}"/bin/sk-sk)
              ${pkgs.kubectl}/bin/kubectl exec -ti "$pod" -c "$container" -- sh
            end
          end
          commandline -f repaint
        end
        bind \ce kube-pod-exec
        if bind -M insert > /dev/null 2>&1
          bind -M insert \ce kube-pod-exec
        end

        function gcloud-project-select -d "Select gcloud project"
          set proj (${pkgs.google-cloud-sdk}/bin/gcloud projects list | tail -n +2 | ${pkgs.gawk}/bin/awk '{print $1}' | ${pkgs.skim}/bin/sk)
          gcloud config set project $proj
        end
        bind \cw gcloud-project-select

        if bind -M insert > /dev/null 2>&1
          bind -M insert \cw gcloud-project-select
        end
      end
    '';
    loginShellInit = ''
      if test "$DISPLAY" = ""; and test (tty) = /dev/tty1 || test (tty) = /dev/tty2; and test "$XDG_SESSION_TYPE" = "tty"
        ${launcher}
      end
    '';
  };
}
