{ config, lib, pkgs, ... }:

let
  cfg = config.services.emacs-gcal-sync;
  inherit (lib) mapAttrsToList makeBinPath mkEnableOption
    mkOption mkIf types concatStringsSep;
  packages = [ pkgs.curl pkgs.gawk ];
in
{
  options.services.emacs-gcal-sync = {
    enable =
      mkEnableOption
        "enable gcal sync to emacs org agenda";

    outputDirectory = mkOption {
      type = types.str;
      example = "/home/user/files/org";
      description = ''
        Where to write the org files.
      '';
    };

    calendars = mkOption {
      type = types.attrsOf types.lines;
      example = {
        cal1 = ''
          echo https://calendar.google.com/calendar/ical/someone@example.com/private-123454656/basic.ics
        '';
      };
      description = ''
        The calendars to fetch and convert to org files. Each attribute is expected to contain a shell script
        snippet which will return the url to the calendar.
      '';
    };

    interval = mkOption {
      type = types.str;
      default = "*:0/5";
      example = "hourly";
      description = ''
        Sync calendars at this interval. The default is every 5 minutes.
        See <literal>man systemd.time</literal> for
        more information on the syntax.
      '';
    };
  };

  config = mkIf cfg.enable {

    systemd.user.services.emacs-gcal-sync =
      let
        calendarsSync = concatStringsSep "\n" (mapAttrsToList
          (name: value: ''
            curl "$(${value})" | awk -f ${../files/ical2org.awk} > ${cfg.outputDirectory}/${name}.org
          '')
          cfg.calendars);
        emacs-gcal-sync = pkgs.writeStrictShellScriptBin "emacs-gcal-sync" ''
          export PATH=${makeBinPath packages}''${PATH:+:''${PATH}}
          mkdir -p ${cfg.outputDirectory}
          ${calendarsSync}
        '';
      in
      {
        Unit.Description = "Sync Google Calendars to Emacs Org Agenda";
        Service.ExecStart = "${emacs-gcal-sync}/bin/emacs-gcal-sync";
      };

    systemd.user.timers.emacs-gcal-sync = {
      Unit.Description = "Sync Google Calendars to Emacs Org Agenda";
      Timer = {
        OnCalendar = cfg.interval;
        Unit = "emacs-gcal-sync.service";
      };
      Install.WantedBy = [ "timers.target" ];
    };
  };
}
