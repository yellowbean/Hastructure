{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs ; [
    cabal-install
    cabal2nix
  ];
  LANG = "en_US.UTF-8";
  LC_ALL = "en_US.UTF-8";
  # LD_LIBRARY_PATH = "${pkgs.gcc.cc.lib}/lib:${pkgs.zeromq}/lib";

  # shellHook = " ";
}
