{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # - flake-utils provides `eachDefaultSystem` which simplifies our flake.
    #   See the `outputsFor` function, which factors out the 'system' at
    #   top-level (as function argument)
    # - flake-compat allows us to provide a shell.nix until
    #   https://github.com/arrterian/nix-env-selector/issues/53 is fixed.
    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-compat.inputs.nixpkgs.follows = "nixpkgs";

    # Rust inputs
    # - crate2nix provides developer environments, unlike the Rust support in nixpkgs.
    # - rust-overlay provides the suite of Rust toolchains we need for IDE
    #   support, etc. It is also fairly easy to switch between different Rust
    #   channels.
    crate2nix.url = "github:kolloch/crate2nix";
    crate2nix.flake = false;
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      # A function that produces Flake outputs for the given system.
      #
      # Define the entire flake inside this function, which factors out the
      # 'system' at top-level var.
      outputsFor = system:
        let
          name = "yubihsm-ed-sign";
          pkgs = import nixpkgs {
            inherit system;
            overlays = rust.overlays;
          };
          mergeDevShells = import ./nix/mergeDevShells.nix { inherit pkgs; };
          rust = import ./nix/rust.nix ({ inherit pkgs; } // inputs);

          # Nix for Rust development environment and build
          #
          # - `(rustProject false).rootCrate` gives the Rust crate, the
          #   `build.lib` attribute of which returns the derivation for the
          #   library in Cargo.toml
          # - `rustProject true` gives the dev shell.
          rustProject = returnShellEnv:
            rust.developPackage {
              inherit returnShellEnv name;
              root = ./rustbits;
            };

          # Nix for Haskell development environment and build
          #
          # - `haskellProject false` gives the build derivation.
          # - `haskellProject true` gives the dev shell.
          haskellProject = returnShellEnv:
            # NOTE: developPackage internally uses callCabal2nix
            pkgs.haskellPackages.developPackage {
              inherit returnShellEnv name;
              root = ./.;
              withHoogle = false;
              overrides = self: super: with pkgs.haskell.lib; {
                # Use callCabal2nix to override Haskell dependencies here
                # cf. https://tek.brick.do/K3VXJd8mEKO7
                # Example: 
                # > NanoID = self.callCabal2nix "NanoID" inputs.NanoID { };
                # Assumes that you have the 'NanoID' flake input defined.
              };
              modifier = drv:
                pkgs.haskell.lib.overrideCabal drv (drv: {
                  extraLibraries = (drv.extraLibraries or [ ]) ++ [
                    (rustProject false).rootCrate.build.lib
                  ];
                  buildTools = with pkgs.haskellPackages; (drv.buildTools or [ ]) ++ pkgs.lib.lists.optionals returnShellEnv [
                    cabal-install
                    ghcid
                    haskell-language-server
                  ];
                });
            };
        in
        {
          devShell =
            mergeDevShells
              [
                (haskellProject true)
                (rustProject true)
              ];

          packages = {
            yubihsm-ed-sign-rust = (rustProject false).rootCrate.build.lib;
            yubihsm-ed-sign-haskell = haskellProject false;
          };

          defaultPackage = self.packages.${system}.yubihsm-ed-sign-haskell;
        };
    in
    inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] outputsFor;
}
