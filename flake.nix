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
        inputs.cl-nix-lite.flakeModules.default
        inputs.treefmt-nix.flakeModule
        ({
          perSystem =
            {
              config,
              lib,
              self',
              system,
              pkgs,
              ...
            }:
            {
              packages.default = pkgs.lispPackagesLite.cl-markdown;
              cl-nix-lite = {
                enable = true;
                checks.enable = true;
                packages = import ./packages.nix { inherit lib; };
                setPkgs = true;
              };
              treefmt.programs.nixfmt = {
                enable = true;
                strict = true;
              };
            };
        })
      ];
    };
}
