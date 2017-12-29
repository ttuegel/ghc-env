import <ghc-env> {
  haskellPackages = pkgs: pkgs.haskell.packages.ghc822;
  inputs = hsPackages: with hsPackages;
    [
      async
      attoparsec
      containers
      formatting
      io-streams
      nix-derivation
      path
      path-io
      text
    ];
}
