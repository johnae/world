{ pkgs, config, lib, options, ... }:
let
  firefox = pkgs.firefox.override {
    extraNativeMessagingHosts = [ pkgs.tridactyl-native ];
  };
in
{
  programs.firefox = {
    enable = true;
    package = firefox;
    extensions = with pkgs.nur.repos.rycee.firefox-addons; [
      https-everywhere
      tridactyl
      adsum-notabs
      ublock-origin
      react-devtools
      duckduckgo-privacy-essentials
      privacy-badger
    ];
    profiles = {
      default = {
        settings = {
          "browser.startup.homepage" = "https://duckduckgo.com";
          "browser.search.region" = "SE";
          "browser.search.isUS" = false;
          "distribution.searchplugins.defaultLocale" = "sv-SE";
          "general.useragent.locale" = "sv-SE";
          "browser.bookmarks.showMobileBookmarks" = true;
          "browser.tabs.opentabfor.middleclick" = false;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "media.peerconnection.enabled" = true;
          "gfx.webrender.all" = true;
          "media.ffmpeg.vaapi.enabled" = true;
          "media.ffvpx.enabled" = true;
        };

        userChrome = ''
          #TabsToolbar {
          visibility: collapse;
          }
          #nav-bar {
          visibility: collapse;
          }
        '';
      };
    };
  };
}
