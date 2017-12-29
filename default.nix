{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs.pkgs;

let
  env = import ./ghc.environment.nix;
in

stdenv.mkDerivation {
  name = "ghc-env-0";

  src =
    builtins.filterSource
    (path: type: lib.hasSuffix ".hs" path)
    ./.;

  buildInputs = [ env.ghc ];

  postPatch = ''
    substituteInPlace ghc-env.hs \
      --replace 'make-ghc-env.nix' '${./make-ghc-env.nix}'
  '';

  configurePhase = ''
    runHook preConfigure

    cp ${env} ".$(stripHash ${env})"

    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild

    ghc -Wall --make ghc-env.hs

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p "$out/bin"
    cp ghc-env "$out/bin"

    runHook postInstall
  '';
}

