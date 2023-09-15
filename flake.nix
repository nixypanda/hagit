{
  description = "Hagit";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];
        pkgs = import nixpkgs { inherit system overlays; };

        hPkgs = pkgs.haskell.packages."ghc946";

        myDevTools = [
          hPkgs.ghc # GHC compiler in the desired version (will be available on PATH)
          hPkgs.ghcid # Continuous terminal Haskell compile checker
          hPkgs.ormolu # Haskell formatter
          hPkgs.hlint # Haskell codestyle checker
          hPkgs.hoogle # Lookup Haskell documentation
          hPkgs.haskell-language-server # LSP server for editor
          # hPkgs.implicit-hie # auto generate LSP hie.yaml file from cabal
          # hPkgs.retrie # Haskell refactoring tool
          # hPkgs.cabal-install
          stack-wrapped
          pkgs.zlib # External C library needed by some Haskell packages
        ];

        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [ pkgs.stack ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in with pkgs; {
        devShell = mkShell { buildInputs = [ myDevTools ]; };
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath myDevTools;
      });
}
