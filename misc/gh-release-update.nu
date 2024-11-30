#!/usr/bin/env nu

let headers = if ('GH_TOKEN' in $env) {
  ["Authorization" $"Bearer ($env.GH_TOKEN)"]
} else { [] }

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
  print $"update-release-url: ($url)"
  if ($url | str contains "/releases/") {
    print $"url contains /releases/"
    let url_parts = ($url | parse --regex 'https://github.com/(?P<owner>[a-zA-Z0-9-]+)/(?P<repo>[a-zA-Z0-9-]+)/releases/download/v?(?P<version>[.0-9]+).*/(?P<file>[a-zA-Z0-9-_.]+)$')
    let api_url = $"https://api.github.com/repos/($url_parts.owner | to text)/($url_parts.repo | to text)/releases"
    print $"api_url: ($api_url)"
    let releases = (http get --headers $headers $api_url)
    let new_version = ($releases | filter { |release| $release.draft == false and $release.prerelease == false } | first | get name | str replace "v" "")
    print $"new_version: ($new_version)"
    let new_url = $url | str replace --all ($url_parts.version | to text) $new_version
    open flake.nix | str replace $url $new_url | save -f flake.nix
  } else if ($url | str contains "/raw.githubusercontent.com/") {
    print $"url contains /raw.githubusercontent.com/"
    let url_parts = ($url | parse --regex 'https://raw.githubusercontent.com/(?P<owner>[a-zA-Z0-9-]+)/(?P<repo>[a-zA-Z0-9-]+)/v?(?P<version>[.0-9]+).*/(?P<file>[a-zA-Z0-9-_.]+)$')
    let api_url = $"https://api.github.com/repos/($url_parts.owner | to text)/($url_parts.repo | to text)/releases"
    print $"api_url: ($api_url)"
    let releases = (http get --headers $headers $api_url)
    let new_version = ($releases | filter { |release| $release.draft == false and $release.prerelease == false } | first | get name | str replace "v" "")
    print $"new_version: ($new_version)"
    let new_url = $url | str replace --all ($url_parts.version | to text) $new_version
    open flake.nix | str replace $url $new_url | save -f flake.nix
  } else {
    print $"gh-release-update: unsupported URL ($url), skip"
  }
}

get-eligible-flake-inputs | each { |url| update-release-url $url }
