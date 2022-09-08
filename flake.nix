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

    # For Rust development.
    nci.url = "github:yusdacra/nix-cargo-integration";
    nci.inputs.nixpkgs.follows = "nixpkgs";

    bech32.url = "github:input-output-hk/bech32";
    bech32.flake = false;
  };

  outputs = { self, nixpkgs, nci, ... }@inputs:
    let
      # A function that produces Flake outputs for the given system.
      #
      # Define the entire flake inside this function, which factors out the
      # 'system' at top-level var.
      outputsFor = system:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
          mergeDevShells = import ./nix/mergeDevShells.nix { inherit pkgs; };

          # Subflake for Rust development environment and build
          rustFlake = nci.lib.makeOutputs {
            # Documentation and examples:
            # https://github.com/yusdacra/rust-nix-templater/blob/master/template/flake.nix
            root = ./rustbits;
            overrides = {
              shell = common: prev: {
                packages = prev.packages ++ [
                  pkgs.rust-analyzer
                ];
                env = prev.env ++ [
                  # For downstream projects (eg: Haskell) to access the Rust
                  # library in their runtime tools like repls and language
                  # servers.
                  (nixpkgs.lib.nameValuePair "LD_LIBRARY_PATH" "${rustPackage}/lib")
                ];
              };
            };
          };
          rustPackage = rustFlake.packages.${system}.yubihsm-ed-sign;
          rustShell = rustFlake.devShell.${system};

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
              source-overrides = {
                bech32="${self.inputs.bech32}/bech32";
              };
              overrides = self: super: with pkgs.haskell.lib; {
                # Use callCabal2nix to override Haskell dependencies here
                # cf. https://tek.brick.do/K3VXJd8mEKO7
                # Example:
                # > NanoID = self.callCabal2nix "NanoID" inputs.NanoID { };
                # Assumes that you have the 'NanoID' flake input defined.
                yubihsmedsign = rustPackage;
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
            haskellPackage = haskellProject false;
            haskellShell = haskellProject true;

        in
        {
          devShells = {
            common = pkgs.mkShell {
              buildInputs = with pkgs; [
                yubihsm-shell
                yubihsm-connector
              ];
            };
            rust = rustShell;
            haskell = haskellShell;
            default =
              mergeDevShells
                [
                  self.devShells.${system}.haskell
                  self.devShells.${system}.rust
                  self.devShells.${system}.common
                ];
          };

          packages = {
            rust = rustPackage;
            default = haskellPackage;
          };

          # For commpat with older Nix
          defaultPackage = self.packages.${system}.default;
          devShell = self.devShells.${system}.default;
        };
    in
    inputs.flake-utils.lib.eachSystem [ "x86_64-linux" ] outputsFor;
}
