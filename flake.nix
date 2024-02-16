{
  inputs = {
    common.url = "github:nammayatri/common/ac4b0421da7088877deba240e0b19bf1f8616a86";
    haskell-tools.url = "github:juspay/haskell-tools/de1208e5692004d62960dd30362b0fb430d8f1de";
    haskell-tools.inputs.nixpkgs.follows = "common/nixpkgs";
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      debug = true;
      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          devShell = {
            tools = hp: { haskell-language-server = (pkgs.haskell.lib.compose.disableCabalFlag "fourmolu" pkgs.haskell.packages.ghc927.haskell-language-server).override { hls-fourmolu-plugin = null; }; };
          };
          imports = [inputs.haskell-tools.haskellFlakeProjectModules.output];
          autoWire = [ "packages" "checks" ];
        };
        process-compose = { };
        packages.default = self'.packages.cada;
      };
    };
}