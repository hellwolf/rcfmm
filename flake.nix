{
  description = "Nix flake for the rcfmm monorepo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flakeUtils.url = "github:numtide/flake-utils";
    foundry.url = "github:shazow/foundry.nix/monthly";
  };

  outputs = { self, nixpkgs, flakeUtils, foundry } : flakeUtils.lib.eachDefaultSystem 
  (system:
  let
    pkgs = import nixpkgs { inherit system; };
    minimumInputs = with pkgs; [
      nodejs-18_x
      nodePackages.pnpm
    ];
    packageMathInputs = with pkgs; [
      # sage math for symbolic calculation
      sage
    ];
    packageToyModelInputs = with pkgs; [
      # haskell build tools
      cabal-install
      haskell.compiler.ghc94
      # haskell dev tools
      hlint
      stylish-haskell
      # other tools
      gnuplot
    ];
  in {
    devShells.default = with pkgs; mkShell {
      buildInputs = minimumInputs
        ++ packageMathInputs
        ++ packageToyModelInputs;
    };
  });
}
