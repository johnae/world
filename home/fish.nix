{ pkgs, config, lib, options }:
let
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

  privateSway = withinNetNS "sway" { };
  privateFish = withinNetNS "fish" { };
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
      e = "emacsclient -t -a=";
      em = "emacs -nw";
    };
    shellAliases = {
      k8s-run = "${pkgs.kubectl}/bin/kubectl run tmp-shell --generator=run-pod/v1 --rm -i --tty --image=nixpkgs/nix-unstable --restart=Never --attach -- nix-shell -p bashInteractive --run bash";
    };
    shellInit = with pkgs; ''
      source ${skim}/share/skim/key-bindings.fish
      set fish_greeting
      fish_vi_key_bindings ^ /dev/null

      function fish_user_key_bindings
        skim_key_bindings

        function skim-jump-to-project-widget -d "Show list of projects"
          set -q SK_TMUX_HEIGHT; or set SK_TMUX_HEIGHT 40%
          begin
            set -lx SK_DEFAULT_OPTS "--color=bw --height $SK_TMUX_HEIGHT $SK_DEFAULT_OPTS --tiebreak=index --bind=ctrl-r:toggle-sort $SK_CTRL_R_OPTS +m"
            set -lx dir (${project-select}/bin/project-select ~/Development ~/.config)
            if [ "$dir" != "" ]
              cd $dir
              set -lx file (${fd}/bin/fd -H -E "\.git" . | "${skim}"/bin/sk --color=bw)
              if [ "$file" != "" ]
                emacsclient -t -a= "$file"
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
          set -q SK_TMUX_HEIGHT; or set SK_TMUX_HEIGHT 40%
          begin
            set -lx SK_DEFAULT_OPTS "--color=bw --height $SK_TMUX_HEIGHT $SK_DEFAULT_OPTS --tiebreak=index --bind=ctrl-r:toggle-sort $SK_CTRL_R_OPTS +m"
            set -lx file (${fd}/bin/fd -H -E "\.git" . | "${skim}"/bin/sk)
            if [ "$file" != "" ]
              emacsclient -t -a= "$file"
            end
          end
          commandline -f repaint
        end
        bind \cf skim-jump-to-project-widget

        if bind -M insert > /dev/null 2>&1
          bind -M insert \cf skim-jump-to-file-widget
        end

        function kubectx-select -d "Select kubernetes cluster"
          ${kubectx}/bin/kubectx
        end
        bind \ck kubectx-select

        if bind -M insert > /dev/null 2>&1
          bind -M insert \ck kubectx-select
        end

        function kubens-select -d "Select kubernetes namespace"
          ${kubectx}/bin/kubens
        end
        bind \cn kubectx-select

        if bind -M insert > /dev/null 2>&1
          bind -M insert \cn kubens-select
        end

        function gcloud-project-select -d "Select gcloud project"
          set proj (${google-cloud-sdk}/bin/gcloud projects list | tail -n +2 | ${gawk}/bin/awk '{print $1}' | ${skim}/bin/sk)
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
        export GDK_BACKEND=wayland
        export MOZ_ENABLE_WAYLAND=1
        export XCURSOR_THEME=default
        export QT_STYLE_OVERRIDE=gtk
        export _JAVA_AWT_WM_NONREPARENTING=1
        export XDG_CURRENT_DESKTOP=sway

        clear
        set RUN (echo -e "sway private\texec ${privateSway}\nsway\texec sway\nfish private\texec ${privateFish}\nfish\texec ${pkgs.dbus}/bin/dbus-run-session fish" | \
        ${pkgs.skim}/bin/sk -p "start >> " --inline-info --margin 40%,40% --color=bw --height=40 --no-hscroll --no-mouse --reverse --delimiter='\t' --with-nth 1 | ${pkgs.gawk}/bin/awk -F'\t' '{print $2}')
        eval "$RUN"
      end
    '';
  };
}
