{
  description = "imm flake";

  inputs = {
    # nixpkgs.url = "github:NixOS/nixpkgs/d89d7af1ba23bd8a5341d00bdd862e8e9a808f56";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        # `nix develop`
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs.haskellPackages;
            [ cabal-install cabal-plan ghcid haskell-language-server hlint ormolu pkgs.sqlite-interactive pkgs.zlib ];
        };
      });
}
