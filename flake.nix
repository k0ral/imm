{
  description = "imm flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/8e4fe32876ca15e3d5eb3ecd3ca0b224417f5f17";
    beam.url = "github:haskell-beam/beam/efd464b079755a781c2bb7a2fc030d6c141bbb8a";
    beam.flake = false;
    rss-conduit.url = "github:k0ral/rss-conduit/c1fec73d715fd1c9a95a155e87ba469887b8e543";
    rss-conduit.flake = false;
  };

  outputs = { self, nixpkgs, beam, rss-conduit }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      hlib = pkgs.haskell.lib;

      source = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
      package = with pkgs;
        add-runtime-dependencies (my-haskell-packages.callCabal2nix "imm" source { }) [
          monolith
          pup
          sqliteInteractive
        ];
      executable = hlib.justStaticExecutables package;

      my-haskell-packages = pkgs.haskellPackages.override {
        overrides = hself: hsuper:
          let from-hackage = x: hlib.doJailbreak (hlib.dontCheck (hlib.dontHaddock (hsuper.callHackageDirect x { })));
          in {
            imm = package;
            beam-core = hlib.doJailbreak (hself.callCabal2nix "beam-core" "${beam}/beam-core" { });
            beam-migrate = hlib.doJailbreak (hself.callCabal2nix "beam-core" "${beam}/beam-migrate" { });
            beam-sqlite = hlib.doJailbreak (hself.callCabal2nix "beam-core" "${beam}/beam-sqlite" { });
            rss-conduit = hself.callCabal2nix "rss-conduit" rss-conduit { };
            chronos = from-hackage {
              pkg = "chronos";
              ver = "1.1.1";
              sha256 = "gOHjPrDXgyZjfC/4qp7UwCj9FOopO5vGQaAKkRoCdRg=";
            };
            co-log = from-hackage {
              pkg = "co-log";
              ver = "0.4.0.1";
              sha256 = "r/thdqWagfWFFVntgxytOWIrERUj2WCFqZXyGTA9wxU=";
            };
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
      defaultPackage.x86_64-linux = executable;
      devShell = my-haskell-packages.shellFor {
        packages = p: [ p.imm ];

        buildInputs = [ my-haskell-packages.cabal-install my-haskell-packages.hlint pkgs.sqliteInteractive ];

        withHoogle = false;
      };
    };

}
