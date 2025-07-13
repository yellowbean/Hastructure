{ nixpkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/25.05.tar.gz";
    sha256 = "1915r28xc4znrh2vf4rrjnxldw2imysz819gzhk9qlrkqanmfsxd"; # Replace with actual hash
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
      haskell-language-server
    ];
  };

  installPhase = ''
    cp config.yaml $out/bin/
  };
}
