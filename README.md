# yubihsm-ed-sign

This is Haskell bindings to perform signing using
Ed25519 algorithm used in blockchains such as Cardano.
This is meant to enable using YubiHSM (Yubico
Hardware Security Module) as an offline signing device
to hold the key and sign transactions instead of a wallet.
It uses [yubihsm.rs](https://github.com/iqlusioninc/yubihsm.rs)
YubiHSM Rust community bindings.

## Build instructions

We are still working on a nix-based build. For now,
you can build using `cargo` and `cabal`:

- get those tools (under NixOS that would be `nix-shell -p cabal-install cargo`
- `cd rustbits`
- `cargo build --release`
- `cd `..`
- `cabal build`

### VSCode setup

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix develop -c echo` to sanity check your environment 
- [Open as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - When prompted by VSCode, install the [workspace recommended](https://code.visualstudio.com/docs/editor/extension-marketplace#_workspace-recommended-extensions) extensions
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and then select `shell.nix`. 
        - The extension will ask you to reload VSCode at the end. Do it.

## Directory structure

- `rustbits` is the Rust code
- The project root is the Haskell project that invokes that code
  through FFI
