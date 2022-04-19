{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    dream2nix = {
      url = "github:davhau/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, dream2nix }@inputs:
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
    in
    {
      devShell = forAllSystems (system: pkgs:
        pkgs.mkShell {
          buildInputs = [
            pkgs.cargo
            pkgs.ghc
          ];
        });

      packages = forAllSystems (system: pkgs: {
        yubihsm-ed-sign = (dream2nix.makeFlakeOutputs {
          source = ./rustbits;
        }).packages.${system}.yubihsm-ed-sign;
      });
    };
}