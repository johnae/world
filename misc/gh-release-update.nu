#!/usr/bin/env nu

def get-eligible-flake-inputs [] {
  open flake.nix |
    lines |
    filter { |line| $line | str contains "gh-release-update" } |
    split column -c --regex '\s+' |
    each { |row| 
           $row.column3 | split column -c '"' | each {|row| $row.column1 }
         } |
    flatten
}

def update-release-url [url: string] {
  if ($url | str contains "/releases/") {
    let url_parts = ($url | parse --regex 'https://github.com/(?P<owner>[a-zA-Z-0-9]+)/(?P<repo>[a-zA-Z-0-9]+)/releases/download/v?(?P<version>[.0-9]+).*/(?P<file>[a-zA-Z0-9-.]+)$')
    let api_url = $"https://api.github.com/repos/($url_parts.owner | to text)/($url_parts.repo | to text)/releases"
    let releases = (http get $api_url)
    let new_version = ($releases | filter { |release| $release.draft == false and $release.prerelease == false } | first | get name | str replace "v" "")
    let new_url = $url | str replace ($url_parts.version | to text) $new_version
    open flake.nix | str replace $url $new_url | save -f flake.nix
  } else if ($url | str contains "/raw.githubusercontent.com/") {
    let url_parts = ($url | parse --regex 'https://raw.githubusercontent.com/(?P<owner>[a-zA-Z-0-9]+)/(?P<repo>[a-zA-Z-0-9]+)/v?(?P<version>[.0-9]+).*/(?P<file>[a-zA-Z0-9-.]+)$')
    let api_url = $"https://api.github.com/repos/($url_parts.owner | to text)/($url_parts.repo | to text)/releases"
    let releases = (http get $api_url)
    let new_version = ($releases | filter { |release| $release.draft == false and $release.prerelease == false } | first | get name | str replace "v" "")
    let new_url = $url | str replace ($url_parts.version | to text) $new_version
    open flake.nix | str replace $url $new_url | save -f flake.nix
  } else {
    echo $"gh-release-update: unsupported URL ($url), skip"
  }
}

get-eligible-flake-inputs | each { |url| update-release-url $url }
