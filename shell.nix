with import <nixpkgs> {};
mkShell {
    packages = [
        ghc
        cabal-install
        haskell-language-server
    ];
}
