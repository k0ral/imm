{ nixpkgs ? import <nixpkgs> { } }:

let
  hlib = nixpkgs.haskell.lib;

  source = nixpkgs.nix-gitignore.gitignoreSource [ ] ./.;
  packageWithoutRuntimeDependencies =
    myHaskellPackages.callCabal2nix "imm" source { };
  packageWithRuntimeDependencies =
    addRuntimeDependencies packageWithoutRuntimeDependencies [
      nixpkgs.httpie
      nixpkgs.pup
    ];

  exe = hlib.justStaticExecutables packageWithRuntimeDependencies;

  my-avro = {
    pkg = "avro";
    ver = "0.5.2.0";
    sha256 = "0inznspd7lwrc4z7bz12zrdh75zmyibidyb5yblxd3vmni68dx5c";
  };

  my-atom-conduit = {
    pkg = "atom-conduit";
    ver = "0.8.0.0";
    sha256 = "17p8180dz3kv9ljfhjqspxp6km50xdcgsdnkknz9w3nfbkpk0l25";
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
        avro = fromHackage my-avro;
        atom-conduit = fromHackage my-atom-conduit;
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

    buildInputs = with nixpkgs.haskellPackages; [
      cabal-install
      haskell-ci
      hlint
      ghcid
    ];

    withHoogle = false;
  };

in {
  inherit shell;
  inherit exe;
}
