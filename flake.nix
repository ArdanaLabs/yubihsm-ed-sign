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
            overlays = rustOverlays;
          };
          mergeDevShells = import ./nix/mergeDevShells.nix { inherit pkgs; };

          # Nix for Rust development environment and build
          rustProject =
            let
              # This is provides Haskell's `callCabal2Nix` but for Rust.
              # cf. https://github.com/kolloch/crate2nix/issues/110
              inherit (import "${inputs.crate2nix}/tools.nix" { inherit pkgs; })
                generatedCargoNix;
            in
            pkgs.callPackage (generatedCargoNix { inherit name; src = ./rustbits; })
              {
                buildRustCrate = null; # https://github.com/kolloch/crate2nix/pull/178#issuecomment-820692187
                # Individual crate overrides go here
                # Example: https://github.com/balsoft/simple-osd-daemons/blob/6f85144934c0c1382c7a4d3a2bbb80106776e270/flake.nix#L28-L50
                defaultCrateOverrides = pkgs.defaultCrateOverrides // {
                  # The project crate itself is overriden here. Typically we
                  # configure non-Rust dependencies (see below) here.
                  ${name} = oldAttrs: rustProjectDeps;
                };
              };
          # Rust release channel to use.
          # https://rust-lang.github.io/rustup/concepts/channels.html
          rustChannel = "stable";
          rustOverlays =
            [
              inputs.rust-overlay.overlay
              (self: super: {
                # Because rust-overlay bundles multiple rust packages into one
                # derivation, specify that mega-bundle here, so that crate2nix
                # will use them automatically.
                rustc = self.rust-bin.${rustChannel}.latest.default;
                cargo = self.rust-bin.${rustChannel}.latest.default;
              })
            ];
          rustProjectDeps = {
            # Configuration for the non-Rust dependencies
            buildInputs = with pkgs; [ openssl.dev ];
            nativeBuildInputs = with pkgs; [ rustc cargo pkgconfig ];
          };
          rustProjectLib = rustProject.rootCrate.build.lib;
          rustProjectShell =
            pkgs.mkShell {
              inputsFrom = [ rustProjectLib ];
              buildInputs = rustProjectDeps.buildInputs ++ (with pkgs;
                # Tools you need for development go here.
                [
                  cargo-watch
                  pkgs.rust-bin.${rustChannel}.latest.rust-analysis
                  pkgs.rust-bin.${rustChannel}.latest.rls
                ]);
              RUST_SRC_PATH = "${pkgs.rust-bin.${rustChannel}.latest.rust-src}/lib/rustlib/src/rust/library";
            };

          # Nix for Haskell development environment and build
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
                    rustProjectLib
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
                rustProjectShell
              ];

          packages = {
            yubihsm-ed-sign-rust = rustProjectLib;
            yubihsm-ed-sign-haskell = haskellProject false;
          };

          defaultPackage = self.packages.${system}.yubihsm-ed-sign-haskell;
        };
    in
    inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] outputsFor;
}
