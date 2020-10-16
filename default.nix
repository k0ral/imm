{ nixpkgs ? import sources.nixpkgs {}
, sources ? import ./nix/sources.nix
}:

let
  hlib = nixpkgs.haskell.lib;

  source = nixpkgs.nix-gitignore.gitignoreSource [ ] ./.;
  packageWithoutRuntimeDependencies =
    myHaskellPackages.callCabal2nix "imm" source { };
  packageWithRuntimeDependencies = with nixpkgs;
    addRuntimeDependencies packageWithoutRuntimeDependencies [
      monolith
      pup
      sqliteInteractive
    ];

  exe = hlib.justStaticExecutables packageWithRuntimeDependencies;

  myHaskellPackages = nixpkgs.haskellPackages.override {
    overrides = hself: hsuper:
      let
        fromHackage = x:
          hlib.dontCheck (hlib.dontHaddock (hsuper.callHackageDirect x { }));
      in {
        imm = packageWithRuntimeDependencies;
        atom-conduit = hself.callCabal2nix "atom-conduit" sources.atom-conduit { };
        beam-core = hself.callCabal2nix "beam-core" "${sources.beam}/beam-core" {};
        beam-migrate = hself.callCabal2nix "beam-core" "${sources.beam}/beam-migrate" {};
        beam-sqlite = hself.callCabal2nix "beam-core" "${sources.beam}/beam-sqlite" {};
        rss-conduit = hself.callCabal2nix "rss-conduit" sources.rss-conduit {};
      };
  };

  addRuntimeDependencies = drv: xs:
    hlib.overrideCabal drv (drv: {
      buildDepends = (drv.buildDepends or [ ]) ++ [ nixpkgs.makeWrapper ];
      postInstall = ''
        ${drv.postInstall or ""}
        for exe in "$out/bin/"* ; do
          wrapProgram "$exe" --prefix PATH ":" \
            ${nixpkgs.lib.makeBinPath xs}
        done
      '';
    });

  shell = myHaskellPackages.shellFor {
    packages = p: [ p.imm ];

    buildInputs = with nixpkgs; [
      haskellPackages.cabal-install
      haskellPackages.haskell-ci
      haskellPackages.hlint
      haskellPackages.ghcid
      sqliteInteractive
    ];

    withHoogle = false;
  };

in {
  inherit shell;
  inherit exe;
}
