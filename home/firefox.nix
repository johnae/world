{ pkgs, config, lib, options }:
{
  programs.firefox = {
    enable = true;
    package = pkgs.firefox-pipewire;
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
