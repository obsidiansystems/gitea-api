{ }:
let
  nixpkgs = import ./generator/nixpkgs {};
  inherit (nixpkgs) lib;
  inherit (nixpkgs.haskell.lib) doJailbreak dontCheck markUnbroken overrideCabal;
  filteredSource = builtins.fetchGit ./.;
  ghc = nixpkgs.haskell.packages.ghc965.override {
  };
in
  ghc.callCabal2nix "gitea-api" filteredSource {}

