# Much of this is based on https://github.com/srid/rust-nix-template
#
# Once dream2nix gets support for Rust dev shells, we may switch over to it.
# Until then crate2nix along with the oxalica overlay provides a desirable
# development environment for Rust based on Nix.
{ pkgs
, # Rust release channel to use.
  # https://rust-lang.github.io/rustup/concepts/channels.html
  rustChannel ? "stable"
, rust-overlay
, crate2nix
, ...
}:

rec {
  overlays =
    [
      rust-overlay.overlay
      (self: super: {
        # Because rust-overlay bundles multiple rust packages into one
        # derivation, specify that mega-bundle here, so that crate2nix
        # will use them automatically.
        rustc = self.rust-bin.${rustChannel}.latest.default;
        cargo = self.rust-bin.${rustChannel}.latest.default;
      })
    ];

  # Like nixpkgs' pkgs.haskellPackages.developPackage but for Rust.
  developPackage =
    { name
    , root
    , returnShellEnv ? false
    }:
    let
      rustProject =
        let
          # This is provides Haskell's `callCabal2Nix` but for Rust.
          # cf. https://github.com/kolloch/crate2nix/issues/110
          inherit (import "${crate2nix}/tools.nix" { inherit pkgs; })
            generatedCargoNix;
        in
        pkgs.callPackage (generatedCargoNix { inherit name; src = root; })
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
      rustProjectDeps = {
        # Configuration for the non-Rust dependencies
        buildInputs = with pkgs; [ openssl.dev ];
        nativeBuildInputs = with pkgs; [ rustc cargo pkgconfig ];
      };
      rustProjectShell =
        pkgs.mkShell {
          inputsFrom = [ rustProject.rootCrate.build ];
          buildInputs = rustProjectDeps.buildInputs ++ (with pkgs;
            # Tools you need for development go here.
            [
              cargo-watch
              pkgs.rust-bin.${rustChannel}.latest.rust-analysis
              pkgs.rust-bin.${rustChannel}.latest.rls
            ]);
          RUST_SRC_PATH = "${pkgs.rust-bin.${rustChannel}.latest.rust-src}/lib/rustlib/src/rust/library";
        };
    in
    if returnShellEnv
    then rustProjectShell
    else rustProject;
}
