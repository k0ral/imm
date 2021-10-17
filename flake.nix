{
  description = "imm flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/31ffc50c571e6683e9ecc9dbcbd4a8e9914b4497";
    flake-utils.url = "github:numtide/flake-utils/2ebf2558e5bf978c7fb8ea927dfaed8fefab2e28";
    beam.url = "github:haskell-beam/beam/efd464b079755a781c2bb7a2fc030d6c141bbb8a";
    beam.flake = false;
  };

  outputs = { self, nixpkgs, flake-utils, beam }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hlib = pkgs.haskell.lib;
        packageName = "imm";

        source = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
        package = with pkgs;
          add-runtime-dependencies (my-haskell-packages.callCabal2nix packageName source { }) [
            monolith
            pup
            sqliteInteractive
          ];

        my-haskell-packages = pkgs.haskellPackages.override {
          overrides = hself: hsuper:
            let from-hackage = x: hlib.doJailbreak (hlib.dontCheck (hlib.dontHaddock (hsuper.callHackageDirect x { })));
            in {
              imm = package;
              beam-core = hlib.doJailbreak (hself.callCabal2nix "beam-core" "${beam}/beam-core" { });
              beam-migrate = hlib.doJailbreak (hself.callCabal2nix "beam-core" "${beam}/beam-migrate" { });
              beam-sqlite = hlib.doJailbreak (hself.callCabal2nix "beam-core" "${beam}/beam-sqlite" { });
            };
        };

        add-runtime-dependencies = drv: xs:
          hlib.overrideCabal drv (drv: {
            buildDepends = (drv.buildDepends or [ ]) ++ [ pkgs.makeWrapper ];
            postInstall = ''
              ${drv.postInstall or ""}
              for exe in "$out/bin/"* ; do
                wrapProgram "$exe" --prefix PATH ":" \
                  ${nixpkgs.lib.makeBinPath xs}
              done
            '';
          });

      in {
        defaultPackage = self.packages.${system}.${packageName};

        packages.${packageName} = hlib.justStaticExecutables package;

        devShell = my-haskell-packages.shellFor {
          packages = p: [ p.${packageName} ];

          buildInputs = with my-haskell-packages; [
            cabal-install
            ghcid
            hlint
            pkgs.sqliteInteractive
          ];

          inputsFrom = builtins.attrValues self.packages.${system};
          withHoogle = false;
        };

      });
}
