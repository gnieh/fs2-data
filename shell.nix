{
  pkgs ? import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs/";
    ref = "refs/tags/24.05";
  }) {}, 
  javaSpec ? "jdk21",
}:
pkgs.mkShell {
  name = "fs2-data-env";
  buildInputs = [ pkgs.${javaSpec} pkgs.ammonite pkgs.coursier pkgs.sbt ];
}
