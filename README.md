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

## Directory structure

- `rustbits` is the Rust code
- The project root is the Haskell project that invokes that code
  through FFI
