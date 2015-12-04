{ pkgs ? import <nixpkgs> {} }:

let
  env = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install ghc ghc-paths hlint text aeson
    vector containers syb
    (callPackage ../broker { })
  ]);

in pkgs.stdenv.mkDerivation {
  name = "monto-haskell-services";
  version = "0.0.1";
  src = ./.;
  buildInputs = [ env ];
  shellHook = ''
    export NIX_GHC="${env}/bin/ghc"
    export NIX_GHCPKG="${env}/bin/ghc-pkg"
    export NIX_GHC_DOCDIR="${env}/share/doc/ghc/html"
    export NIX_GHC_LIBDIR=$( $NIX_GHC --print-libdir )
  '';
}
