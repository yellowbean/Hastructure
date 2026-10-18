{ nixpkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/4c7870105e7f1fdf9c48688c8d7efc21abf0688a.tar.gz";
    sha256 = "sha256-ajQuqcxnj6RYnwCjA/9FTCnrdR2+BDzstE8UembXf60=";
  }) {}
}:
let
  # Use the Haskell package set from nixpkgs
  haskellPackages = nixpkgs.haskellPackages;

  # Define the project
  hastructure = haskellPackages.callCabal2nix "Hastructure" ./. {};
in
{
  # Expose the project derivation
  hastructure = hastructure;

  # Development shell for interactive work
  shell = haskellPackages.shellFor {
    packages = p: [hastructure];
    buildInputs = with haskellPackages; [
      cabal-install
      ghc
    ];
  };
}
