let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/89c49874fb15f4124bf71ca5f42a04f2ee5825fd.tar.gz") { };
  ghcVersion = "ghc965";
  ghcWithPackages = pkgs.haskell.packages.${ghcVersion}.ghcWithPackages (pkgs: with pkgs; [
    haskell-language-server
    hoogle
  ]);
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    ghcWithPackages
    stack
    zlib.dev
  ];
}
