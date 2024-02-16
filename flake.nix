{
  inputs = {
    classyplate.url = "github:Chaitanya-nair/classyplate/46f5e0e7073e1d047f70473bf3c75366a613bfeb";
    classyplate.flake = false;
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    nixpkgs.url = "github:nixos/nixpkgs/43e3b6af08f29c4447a6073e3d5b86a4f45dd420";
    systems.url = "github:nix-systems/default";
    references.url = "github:eswar2001/references/35912f3cc72b67fa63a8d59d634401b79796469e";
    references.flake = true;
    haskell-tools.url = "github:eswar2001/haskell-tools/59e6168555d466237e27aabc794d5f4040b5ca8b";
  };
  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake { inputs = inputs // { inherit (inputs) nixpkgs nixpkgs-latest; }; } {
      systems = import inputs.systems;
      imports = [inputs.haskell-flake.flakeModule];

      perSystem = {
        self',
        pkgs,
        lib,
        config,
        ...
      }: {
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc8107;
          packages = {
            references.source = inputs.references;
            classyplate.source = inputs.classyplate;
            haskell-tools-parser.source = inputs.haskell-tools + /src/parser/haskell-tools-parser.cabal;
          };
        };
        packages.default =  self'.packages.cada;
      };
    };
}