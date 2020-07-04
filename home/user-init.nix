{ pkgs, config, lib, options }:

with pkgs; {
  services.user-init = {
    enable = true;
    target = [ "sway-session.target" ];
    script = ''
      PATH=${git}/bin:${btrfs-progs}/bin''${PATH:+:}$PATH
      export PATH
      cd ~
      for vol in Sync Downloads; do
        if [ ! -e "$vol" ]; then
          echo Creating "$vol" btrfs subvolume
          btrfs sub create "$vol"
        else
          echo "$vol" already exists
        fi
      done
      mkdir -p Development Photos Pictures Mount Documents Videos
    '';
  };
}
