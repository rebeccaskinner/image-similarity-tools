{ pkgs ? import <nixpkgs> {}
, hsPkgs ? pkgs.haskellPackages
, returnShellEnv ? false
}:
hsPkgs.developPackage {
  name = "perceptual-hash";
  root = pkgs.nix-gitignore.gitignoreSourcePure
    [ "dist-newstyle"
      ".*#"
      ".git"
      "test-data"
    ] ./.;
  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs:
    let
      prevTools = attrs.buildTools or [];
      requiredTools = [pkgs.ffmpeg];
    in
      { buildTools = prevTools ++ requiredTools;
      }
  );
  inherit returnShellEnv;
}
