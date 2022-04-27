{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    dream2nix = {
      url = "github:davhau/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    flake-compat.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, dream2nix, ... }@inputs:
    let
      version = builtins.substring 0 8 self.lastModifiedDate;
      supportedSystems = [ "x86_64-linux" ];
      forSystems = systems: f:
        nixpkgs.lib.genAttrs systems
        (system: f system nixpkgs.legacyPackages.${system});
      forAllSystems = forSystems supportedSystems;
      nixpkgsFor = forAllSystems (system: pkgs: import nixpkgs { inherit system; overlays = [ ]; });
      dream2nix = inputs.dream2nix.lib2.init {
        systems = supportedSystems;
        config = {
          projectRoot = ./.;
          overridesDirs = [ "${inputs.dream2nix}/overrides" ];
        };
      };
      rustPackage = system: 
        (dream2nix.makeFlakeOutputs {
            source = ./rustbits;
          }).packages.${system}.yubihsm-ed-sign;
      haskellProject = returnShellEnv: forAllSystems (system: pkgs: 
        let 
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
                  # FIXME: This doesn't work. See also the .cabal file.
                  configureFlags = [ " --extra-lib-dirs=${rustPackage system}/lib" ];
                })) 
                (with hp; pkgs.lib.lists.optionals returnShellEnv [
                  # Specify your build/dev dependencies here. 
                  cabal-install
                  ghcid
                  haskell-language-server
                  # Rust build dependencies
                  pkgs.cargo
                ]);
          }
        );
    in
    {
      devShell = haskellProject true;

      packages = forAllSystems (system: _pkgs: {
        yubihsm-ed-sign = rustPackage system; 
      });
    };
}
