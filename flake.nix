{
  inputs = {
    common.url = "github:nammayatri/common/ac4b0421da7088877deba240e0b19bf1f8616a86";
  };
  outputs = inputs:
    inputs.common.lib.mkFlake { inherit inputs; } {
      debug = true;
      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          autoWire = [ "packages" "checks" ];
        };
        process-compose = { };
        packages.default = self'.packages.cada;
        devShells.default = pkgs.mkShell {
          # cf. https://haskell.flake.page/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.pre-commit.devShell
            config.flake-root.devShell
          ];
        };
      };
    };
}