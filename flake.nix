{
  description = "imm flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/574d1eac1c200690e27b8eb4e24887f8df7ac27c";
    utils.url = "github:numtide/flake-utils";
    utils.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        # `nix develop`
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs.haskellPackages;
            [ cabal-install ghcid haskell-language-server hlint fourmolu pkgs.sqlite-interactive pkgs.zlib ];
        };
      });
}
