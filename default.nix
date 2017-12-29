/*

   Copyright 2017 Thomas Tuegel

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

*/

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

