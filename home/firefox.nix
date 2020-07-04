{ pkgs, config, lib, options }:
let
  firefox-nightly = pkgs.wrapFirefox
    pkgs.latest.firefox-nightly-bin { browserName = "firefox"; };
  #firefox-nightly = pkgs.wrapFirefox
  #  (pkgs.firejailed {
  #    package = pkgs.latest.firefox-nightly-bin;
  #    ignore = [ "nou2f" ];
  #  }
  #  ) { browserName = "firefox"; };
in
{
  programs.firefox = {
    enable = true;
    package = firefox-nightly;
    profiles = {
      default = {
        settings = {
          "browser.startup.homepage" = "about:home";
          "browser.search.region" = "SE";
          "browser.search.isUS" = false;
          "distribution.searchplugins.defaultLocale" = "sv-SE";
          "general.useragent.locale" = "sv-SE";
          "browser.bookmarks.showMobileBookmarks" = true;
          "browser.tabs.opentabfor.middleclick" = false;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "media.peerconnection.enabled" = true;
        };

        userChrome = ''
          #TabsToolbar {
          visibility: collapse;
          }
        '';
      };
    };
  };
}
