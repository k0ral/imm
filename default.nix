{ nixpkgs ? import <nixpkgs> { } }:

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

  my-atom-conduit = {
    pkg = "atom-conduit";
    ver = "0.9.0.0";
    sha256 = "11x41fxj0g93kg46z06i1kxfms84wqbad41i7fnm7ixpw4n464pp";
  };

  my-beam = nixpkgs.fetchFromGitHub {
    owner = "haskell-beam";
    repo = "beam";
    rev = "3bde0615b6eccfe5ca44ed907b79a3cd74eee33f";
    sha256 = "0fy8p70pxhrllr2njwxa8lijf35hlds2zkhh7fkkv5b3i34jqx29";
  };

  my-rss-conduit = {
    pkg = "rss-conduit";
    ver = "0.6.0.0";
    sha256 = "1gdq1bv1ayx9qx9ppwld8b5qjfsq1pgkqd29qni3jcldw9w4wfb8";
  };

  myHaskellPackages = nixpkgs.haskellPackages.override {
    overrides = hself: hsuper:
      let
        fromHackage = x:
          hlib.dontCheck (hlib.dontHaddock (hsuper.callHackageDirect x { }));
      in {
        imm = packageWithRuntimeDependencies;
        atom-conduit = fromHackage my-atom-conduit;
        beam-core = hself.callCabal2nix "beam-core" "${my-beam}/beam-core" {};
        beam-migrate = hself.callCabal2nix "beam-core" "${my-beam}/beam-migrate" {};
        beam-sqlite = hself.callCabal2nix "beam-core" "${my-beam}/beam-sqlite" {};
        rss-conduit = fromHackage my-rss-conduit;
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
