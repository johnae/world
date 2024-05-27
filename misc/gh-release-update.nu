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
    let url_parts = ($url | split column -c "/" | get 0)
    let last_col = ($url_parts | columns) | last
    let owner = ($url_parts | get column3)
    let repo = ($url_parts | get column4)
    let version = ($url_parts | get column7 | str replace "v" "")
    let file = ($url_parts | $last_col)
    let api_url = $"https://api.github.com/repos/($owner)/($repo)/releases"
    echo $api_url
    let releases = (http get $api_url)
    let new_version = ($releases | filter { |release| $release.draft == false and $release.prerelease == false } | first | get name | str replace "v" "")
    let new_url = $url | str replace $version $new_version
    open flake.nix | str replace $url $new_url | save -f flake.nix
  } else if ($url | str contains "/raw.githubusercontent.com/") {
    let url_parts = ($url | split column -c "/" | get 0)
    let last_col = ($url_parts | columns) | last
    let owner = ($url_parts | get column3)
    let repo = ($url_parts | get column4)
    let version = ($url_parts | get column5 | str replace "v" "")
    let file = ($url_parts | $last_col)
    let api_url = $"https://api.github.com/repos/($owner)/($repo)/releases"
    echo $api_url
    let releases = (http get $api_url)
    let new_version = ($releases | filter { |release| $release.draft == false and $release.prerelease == false } | first | get name | str replace "v" "")
    let new_url = $url | str replace $version $new_version
    open flake.nix | str replace $url $new_url | save -f flake.nix
  } else {
    echo $"gh-release-update: unsupported URL ($url), skip"
  }
}

get-eligible-flake-inputs | each { |url| update-release-url $url }
