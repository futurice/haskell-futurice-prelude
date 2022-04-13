{
  description = "The futurice prelude";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          futuricePrelude =
            final.haskell-nix.project' {
              src = ./.;
              projectFileName = "cabal.project";
              compiler-nix-name = "ghc8107";
              plan-sha256 = "1fvgzivzk3z8xc8l17rmpswqbj500qplm670vimhaq3zsx3n7kys";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                hlint = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.futuricePrelude.flake { };
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."futurice-prelude:lib:futurice-prelude";
    });
}
