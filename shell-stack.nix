{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = with pkgs ; [
    stack
    zlib
  ];
  LANG = "en_US.UTF-8";
  LC_ALL = "en_US.UTF-8";
  # LD_LIBRARY_PATH = "${pkgs.gcc.cc.lib}/lib:${pkgs.zeromq}/lib";

  # shellHook = " ";
}
