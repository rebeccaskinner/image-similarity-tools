{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {self, nixpkgs, flake-utils}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = pkgs.haskellPackages;
        shellEnv = import ./default.nix { inherit pkgs hsPkgs;  returnShellEnv = true; };
        lib = import ./default.nix { inherit pkgs hsPkgs; returnShellEnv = false; };
      in {
        packages.default = lib;
        devShells.default = pkgs.mkShell {
          inputsFrom = [shellEnv];
          buildInputs = lib.buildInputs;
          shellHook = ''
            cabal update
          '';
        };
      });
  nixConfig.bash-prompt = "\\u@\\h:\\W (nix) λ ";
}
