{ nixpkgs ? import <nixpkgs> { } }:

let
  hlib = nixpkgs.haskell.lib;

  thisPackage =
    addRuntimeDependencies (myHaskellPackages.callCabal2nix "imm" ./. { }) [
      nixpkgs.httpie
      nixpkgs.pup
    ];

  my-atom-conduit = {
    pkg = "atom-conduit";
    ver = "0.8.0.0";
    sha256 = "17p8180dz3kv9ljfhjqspxp6km50xdcgsdnkknz9w3nfbkpk0l25";
  };

  my-opml-conduit = {
    pkg = "opml-conduit";
    ver = "0.8.0.0";
    sha256 = "14106j2rr6fk6hjhypm5hp1dk1rlxf7svswbj21ad3l40nx7qm7r";
  };

  my-rss-conduit = {
    pkg = "rss-conduit";
    ver = "0.6.0.0";
    sha256 = "1gdq1bv1ayx9qx9ppwld8b5qjfsq1pgkqd29qni3jcldw9w4wfb8";
  };

  myHaskellPackages = nixpkgs.haskellPackages.override {
    overrides = hself: hsuper: {
      imm = thisPackage;
      atom-conduit = hlib.dontCheck
        (hlib.dontHaddock (hsuper.callHackageDirect my-atom-conduit { }));
      opml-conduit = hlib.dontCheck
        (hlib.dontHaddock (hsuper.callHackageDirect my-opml-conduit { }));
      rss-conduit = hlib.dontCheck
        (hlib.dontHaddock (hsuper.callHackageDirect my-rss-conduit { }));
      msgpack = hlib.doJailbreak hsuper.msgpack;
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

  exe = hlib.justStaticExecutables (myHaskellPackages.imm);

in {
  inherit shell;
  inherit exe;
}
