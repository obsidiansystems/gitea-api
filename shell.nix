let
  pkgs = import ./generator/nixpkgs {};
in
  pkgs.mkShell {
    name = "gitea-api";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
    ];
    inputsFrom = [
      (import ./default.nix {}).env
    ];
  }
