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

    # Crane is used to provide Rust build and development environments.
    #   https://ipetkov.dev/blog/introducing-crane/
    #
    # Eventually, we may want to switch over to the dream2nix-based
    # https://github.com/yusdacra/nix-cargo-integration (whilst still using
    # Crane as the backend) once we figure out how to use it in a multi-language
    # flake like ours.
    crane.url = "github:ipetkov/crane";
    crane.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, crane, ... }@inputs:
    let
      # A function that produces Flake outputs for the given system.
      #
      # Define the entire flake inside this function, which factors out the
      # 'system' at top-level var.
      outputsFor = system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          mergeDevShells = import ./nix/mergeDevShells.nix { inherit pkgs; };

          # Nix for Rust development environment and build
          #
          # - `rustProject false` gives the library crate.
          # - `rustProject true` gives the dev shell.
          rustProject = returnShellEnv:
            let
              src = ./rustbits;
              cargoArtifacts = crane.lib.${system}.buildDepsOnly {
                inherit src;
              };
              rustbits-crate = crane.lib.${system}.buildPackage {
                inherit src cargoArtifacts;
              };
              rustbits-devShell = pkgs.mkShell {
                inputsFrom = [ rustbits-crate ];
                nativeBuildInputs = with pkgs; [
                  cargo
                  rustc
                  rust-analyzer
                ];
                # This is needed for rust-analyzer to work.
                RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
                # For downstream projects (eg: Haskell) to access the rustbits
                # in their runtime tools like repls and language servers.
                LD_LIBRARY_PATH = "${rustbits-crate}/lib";
              };
            in
            if returnShellEnv then rustbits-devShell else rustbits-crate;

          # Nix for Haskell development environment and build
          #
          # - `haskellProject false` gives the build derivation.
          # - `haskellProject true` gives the dev shell.
          haskellProject = returnShellEnv:
            # NOTE: developPackage internally uses callCabal2nix
            pkgs.haskellPackages.developPackage {
              inherit returnShellEnv;
              name = "yubihsm-ed-sign";
              root = ./.;
              withHoogle = false;
              overrides = self: super: with pkgs.haskell.lib; {
                # Use callCabal2nix to override Haskell dependencies here
                # cf. https://tek.brick.do/K3VXJd8mEKO7
                # Example: 
                # > NanoID = self.callCabal2nix "NanoID" inputs.NanoID { };
                # Assumes that you have the 'NanoID' flake input defined.
                yubihsmedsign = rustProject false;
              };
              modifier = drv:
                pkgs.haskell.lib.overrideCabal drv (drv: {
                  buildTools = with pkgs.haskellPackages; (drv.buildTools or [ ]) ++ pkgs.lib.lists.optionals returnShellEnv [
                    cabal-install
                    ghcid
                    haskell-language-server
                  ];
                });
            };
        in
        {
          devShells = {
            haskell = haskellProject true;
            rust = rustProject true;
            default =
              mergeDevShells
                [
                  self.devShells.${system}.haskell
                  self.devShells.${system}.rust
                ];
          };

          packages = {
            yubihsm-ed-sign-rust = rustProject false;
            yubihsm-ed-sign-haskell = haskellProject false;
          };

          # For commpat with older Nix
          defaultPackage = self.packages.${system}.yubihsm-ed-sign-haskell;
          devShell = self.devShells.${system}.default;
        };
    in
    inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] outputsFor;
}
