# 🚀 Declarative Today. Utopia Tomorrow.  

This is my [NixOS](https://nixos.org) configuration repository, managing all my machines and custom packages (i.e., not yet in [nixpkgs](https://github.com/nixos/nixpkgs)). In a way, this repo is my world—hence the name. 🌍  

It's built on [Nix flakes](https://nixos.wiki/wiki/Flakes), providing a declarative and reproducible way to manage systems—kind of like `Cargo.toml` (Rust), `go.mod` (Go), or `package.json` (Node.js), but **language-agnostic** and handling any package or file. This setup ensures **deterministic** builds for both hosts and software.  

For modularity, [flake-parts](https://flake.parts) is used to keep [flake.nix](./flake.nix) clean and structured. 🛠️  

## 🖥️ Bootstrapping a New Machine  

To add a new machine, create a file in the relevant architecture folder under [hosts/](hosts/), copy an existing configuration if needed, and adjust as necessary. Then commit and push your changes. ✅  

### 🔧 Installing NixOS  

1. Download a recent [NixOS installer](https://nixos.org/download.html#nixos-iso) and boot into it.  
2. Enable flakes:  

   ```sh
   mkdir -p /etc/nix
   echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf
   ```

3. Clone this repository:  

   ```sh
   cd /tmp
   git clone https://github.com/johnae/world
   cd world
   ```

4. Install the system:  

   ```sh
   host=eris
   nix build .#"$host"-diskformat
   ./result/bin/diskformat
   nixos-install --flake .#"$host" --no-root-passwd
   chown -R 1337:100 /keep/home/<your-user>
   ```

   Hosts are defined in [hosts/](hosts/).  

   _This setup is highly customized to my workflow. Feel free to use it, but you’ll likely want to tweak it for your needs._  

## 🔄 Updating an Existing Machine  

Updates follow a standard commit/push/pull request workflow. To update a machine:  

```sh
nixos-rebuild switch --flake github:johnae/world --use-remote-sudo
```

Or from a local clone:  

```sh
git clone git@github.com:johnae/world
cd world
nixos-rebuild switch --flake . --use-remote-sudo
```

## 🤖 Automated Updates  

OS and package updates are automated via CI (buildkite). Updates create pull requests where all custom packages and machine configurations are built and tested before merging, helping catch issues before deployment.  

## 📜 License  

[MIT](https://choosealicense.com/licenses/mit)  
