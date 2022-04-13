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
              compiler-nix-name = "ghc902";
              plan-sha256 = "147lxrkx7wrvq06glaxw4xij11rg7lw78vv8dgipqxgsc9q09l3g";

              shell = {
                tools.cabal = {};
                buildInputs = with pkgs; [ haskell-ci ];
                withHoogle = false;
              };
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
