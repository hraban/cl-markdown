{
  inputs = {
    nixpkgs.follows = "cl-nix-lite/nixpkgs";
    cl-nix-lite = {
      url = "github:hraban/cl-nix-lite/v0";
      inputs.systems.follows = "systems";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };

  outputs =
    { self, flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.treefmt-nix.flakeModule
        ({
          perSystem =
            {
              self',
              system,
              pkgs,
              ...
            }:
            {
              _module.args.pkgs = import inputs.nixpkgs {
                inherit system;
                overlays = [
                  inputs.cl-nix-lite.overlays.default
                  (import ./packages.nix { inherit (pkgs) lib; })
                ];
              };
              packages.default = pkgs.lispPackagesLite.cl-markdown;
              treefmt.programs.nixfmt = {
                enable = true;
                strict = true;
              };
            };
        })
      ];
    };
}
