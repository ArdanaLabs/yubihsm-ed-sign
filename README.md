# yubihsm-ed-sign

This is Haskell bindings to perform signing using
Ed25519 algorithm used in blockchains such as Cardano.
This is meant to enable using YubiHSM (Yubico
Hardware Security Module) as an offline signing device
to hold the key and sign transactions instead of a wallet.
It uses [yubihsm.rs](https://github.com/iqlusioninc/yubihsm.rs)
YubiHSM Rust community bindings.

## Build instructions

The Rust and Haskell library can be built using Nix as follows,

```sh
# Rust
nix build .#rust
# Haskell
nix build
```

You can also use `cabal` or `cargo` from inside of `nix develop` shell.  For example,

```sh
nix develop
cd ./rustbits
cargo test
cd ../
cabal build
```

To test the Haskell->Rust integration works, a Cabal executable is provided. You can run it as:

```sh-session
❯ nix run
thread '<unnamed>' panicked at 'could not connect to YubiHSM: Error(Context { kind: ProtocolError, source: Some(Error(Context { kind: ProtocolError, source: Some(Error(Context { kind: UsbError, source: Some(Message("no YubiHSM 2 devices detected")) })) })) })', src/lib.rs:39:49
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
fatal runtime error: failed to initiate panic, error 5
Aborted (core dumped)
```

### VSCode setup

- [Install Nix](https://nixos.org/download.html) & [enable Flakes](https://nixos.wiki/wiki/Flakes)
- Run `nix develop -c echo` to sanity check your environment 
- [Open as single-folder workspace](https://code.visualstudio.com/docs/editor/workspaces#_singlefolder-workspaces) in Visual Studio Code
    - When prompted by VSCode, install the [workspace recommended](https://code.visualstudio.com/docs/editor/extension-marketplace#_workspace-recommended-extensions) extensions
    - <kbd>Ctrl+Shift+P</kbd> to run command "Nix-Env: Select Environment" and then select `shell.nix`. 
        - The extension will ask you to reload VSCode at the end. Do it.

### Development workflows

Some useful development works.

- When editing the Haskell library, run `nix develop -c ghcid` to get fast compile feedback.
- When editing the Haskell executable, run `nix develop -c ghcid -c 'cabal repl exe:yubihsm-ed-sign'` to get fast compile feedback.
  - Add `-T :main` inside of `-c` argument if you also want to run the main entrypoint.
- For Haskell repl, `nix develop -c cabal repl`


## Directory structure

- `rustbits` is the Rust code
- The project root is the Haskell project that invokes that code
  through FFI
