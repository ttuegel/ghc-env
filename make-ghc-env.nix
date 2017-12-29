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

{
  nixpkgs ? import <nixpkgs> {},

  # A function that takes `nixpkgs.pkgs' as its argument and returns
  # a set of Haskell packages.
  haskellPackages ? (pkgs: pkgs.haskellPackages),

  # A function taking haskellPackages as its argument that returns a list of
  # build inputs (preferred), or just a list of build inputs.
  inputs
}:

let
  inherit (nixpkgs.pkgs) lib runCommand stdenv;

  hsPackages = haskellPackages nixpkgs.pkgs;

  ghc = hsPackages.ghc;

  buildInputs = [ ghc ] ++ inputs hsPackages;

  packageDatabase =
    runCommand "ghc-env-package.conf.d"
    { inherit buildInputs; }
    ''
      mkdir -p "$out"

      for pkg in ''${nativePkgs[*]} ''${crossPkgs[*]}
      do
        package_db="$pkg/lib/ghc-${ghc.version}/package.conf.d"
        if [ -d "$package_db" ]
        then
          ln -f -s -t "$out" "$package_db"/*
        fi
      done

      ghc-pkg recache --package-db "$out"
    '';
in

runCommand "ghc.environment.${stdenv.system}-${ghc.version}"
{
  inherit buildInputs;
  passthru = { inherit ghc; };
}
''
  # Clear package database stack.
  echo "clear-package-db" >>"$out"
  echo "package-db ${packageDatabase}" >>"$out"

  # Expose input packages, including all core packages.
  for pkg in $buildInputs
  do
    package_db="$pkg/lib/ghc-${ghc.version}/package.conf.d"
    if [ -d "$package_db" ]
    then
      for conf in "$package_db"/*.conf
      do
        conf="$(basename "$conf")"
        package_id="''${conf%.conf}"
        echo "package-id $package_id" >>"$out"
      done
    fi
  done
''
