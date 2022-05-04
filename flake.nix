{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-compat.inputs.nixpkgs.follows = "nixpkgs";

    # Rust inputs
    crate2nix.url = "github:kolloch/crate2nix";
    crate2nix.flake = false;
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      name = "yubihsmedsign";
      version = builtins.substring 0 8 self.lastModifiedDate;
      supportedSystems = [ "x86_64-linux" ];
      # Rust release channel to use.
      # https://rust-lang.github.io/rustup/concepts/channels.html
      rustChannel = "stable";
      forSystems = systems: f:
        nixpkgs.lib.genAttrs systems
        (system: f system nixpkgs.legacyPackages.${system});
      forAllSystems = forSystems supportedSystems;
      nixpkgsFor = forAllSystems (system: _pkgs: 
        import nixpkgs { 
          inherit system; 
          overlays = [ 
            inputs.rust-overlay.overlay
            (self: super: {
              # Because rust-overlay bundles multiple rust packages into one
              # derivation, specify that mega-bundle here, so that crate2nix
              # will use them automatically.
              rustc = self.rust-bin.${rustChannel}.latest.default;
              cargo = self.rust-bin.${rustChannel}.latest.default;
            })
          ]; 
        }
      );

      # Create the cargo2nix project
      rustProject = system: 
        let 
          pkgs = nixpkgsFor.${system};
          # This is provides Haskell's `callCabal2Nix` but for Rust.
          # cf. https://github.com/kolloch/crate2nix/issues/110
          inherit (import "${inputs.crate2nix}/tools.nix" { inherit pkgs; })
            generatedCargoNix;
        in pkgs.callPackage
            (generatedCargoNix {
              inherit name;
              src = ./rustbits;
            })
            {
              buildRustCrate = null; # https://github.com/kolloch/crate2nix/pull/178#issuecomment-820692187

              # Use debug Cargo profile so that mockhsm will compile.
              release = false;

              # Individual crate overrides go here
              # Example: https://github.com/balsoft/simple-osd-daemons/blob/6f85144934c0c1382c7a4d3a2bbb80106776e270/flake.nix#L28-L50
              defaultCrateOverrides = pkgs.defaultCrateOverrides // {
                # The project crate itself is overriden here. Typically we
                # configure non-Rust dependencies (see below) here.
                ${name} = oldAttrs: rustDeps system;
              };
            };
      rustDeps = system: {
        # Configuration for the non-Rust dependencies
        buildInputs = with nixpkgsFor.${system}; [ openssl.dev ];
        nativeBuildInputs = with nixpkgsFor.${system}; [ rustc cargo pkgconfig ];
      };
      haskellProject = system: returnShellEnv: 
        let 
          pkgs = nixpkgsFor.${system};
          # Change GHC version here.
          hp = pkgs.haskellPackages;
        in
          # NOTE: developPackage internally uses callCabal2nix
          hp.developPackage {
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
            };
            modifier = drv:
              pkgs.haskell.lib.addBuildTools 
                (pkgs.haskell.lib.overrideCabal drv (drv: {
                  extraLibraries = (drv.extraLibraries or []) ++ [
                    ((rustProject system).rootCrate.build.lib)
                  ];
                }))
                (with hp; pkgs.lib.lists.optionals returnShellEnv [
                  # Specify your build/dev dependencies here. 
                  cabal-install
                  ghcid
                  haskell-language-server
                  # Rust build dependencies
                  pkgs.cargo
                  pkgs.rustc
                ]);
          };
    in
    {
      devShell = forAllSystems (system: _pkgs:
        let pkgs = nixpkgsFor.${system};
        in (haskellProject system true).overrideAttrs (oa: {
          inputsFrom = (oa.inputsFrom or []) ++ [self.packages.${system}.yubihsm-ed-sign];
          buildInputs = oa.buildInputs ++ (rustDeps system).buildInputs ++ (with pkgs;
            # Tools you need for development go here.
            [
              nixpkgs-fmt
              cargo-watch
              pkgs.rust-bin.${rustChannel}.latest.rust-analysis
              pkgs.rust-bin.${rustChannel}.latest.rls
            ]);
          RUST_SRC_PATH = "${pkgs.rust-bin.${rustChannel}.latest.rust-src}/lib/rustlib/src/rust/library";
        })
      );

      packages = forAllSystems (system: _pkgs: {
        yubihsm-ed-sign = (rustProject system).rootCrate.build.lib; 
        default = haskellProject system false;
      });

      defaultPackage = forAllSystems (system: _pkgs: self.packages.${system}.default);
    };
}
