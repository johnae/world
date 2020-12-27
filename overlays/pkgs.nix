final: prev:

{

  pushDockerArchive = { image, tag ? null }:
    let
      imageTag = if tag != null then tag else builtins.head (prev.lib.splitString "-" (builtins.baseNameOf image.outPath));
    in
    prev.writeStrictShellScript "pushDockerArchive" ''
      echo pushing ${image.imageName}:${imageTag} 1>&2
      ${prev.skopeo}/bin/skopeo --insecure-policy copy "$@" \
          docker-archive:${image} \
          docker://${image.imageName}:${imageTag} 1>&2
      echo ${image.outPath}
    '';

  sway-unwrapped = prev.callPackage ../pkgs/sway { };
  sway = prev.callPackage
    (final.path + "/pkgs/applications/window-managers/sway/wrapper.nix")
    { };

  swaybg = prev.callPackage ../pkgs/swaybg { };
  swayidle = prev.callPackage ../pkgs/swayidle { };
  swaylock = prev.callPackage ../pkgs/swaylock { };
  swaylock-dope = prev.callPackage ../pkgs/swaylock-dope { };
  wlroots = prev.callPackage ../pkgs/wlroots { };
  blur = prev.callPackage ../pkgs/blur { };

  grim = prev.callPackage ../pkgs/grim { };
  #mako = prev.callPackage ../pkgs/mako { };
  slurp = prev.callPackage ../pkgs/slurp { };

  spotifyd = prev.callPackage ../pkgs/spotifyd { };

  wf-recorder = prev.callPackage ../pkgs/wf-recorder { };
  wl-clipboard = prev.callPackage ../pkgs/wl-clipboard { };
  wl-clipboard-x11 = prev.callPackage ../pkgs/wl-clipboard-x11 { };

  argocd = prev.callPackage ../pkgs/argocd { };
  argocd-ui = prev.callPackage ../pkgs/argocd-ui { };

  persway = prev.callPackage ../pkgs/persway { };

  nixpkgs-fmt = prev.callPackage ../pkgs/nixpkgs-fmt { };
  netns-dbus-proxy = prev.callPackage ../pkgs/netns-dbus-proxy { };
  netns-exec = prev.callPackage ../pkgs/netns-exec { };
  buildkite = prev.callPackage ../pkgs/buildkite { };
  xdg-desktop-portal-wlr = prev.callPackage ../pkgs/xdg-desktop-portal-wlr { };
  my-emacs = prev.callPackage ../pkgs/my-emacs { };
  btr-snap = prev.callPackage ../pkgs/btr-snap { };
  fire = prev.callPackage ../pkgs/fire { };
  mynerdfonts = prev.nerdfonts.override { fonts = [ "JetBrainsMono" "DroidSansMono" ]; };

  rust-analyzer-bin = prev.callPackage ../pkgs/rust-analyzer-bin { };

  #k3s = prev.callPackage ../pkgs/k3s { };

  initialize-user = prev.callPackage ../pkgs/initialize-user { };

  inherit (prev.callPackage ../pkgs/scripts { })
    project-select launch git-credential-pass sk-sk
    sk-run sk-window sk-passmenu add-wifi-network update-wifi-networks
    update-wireguard-keys spotify-play-album spotify-play-track spotify-cmd
    spotify-play-artist spotify-play-playlist
    ;
}
