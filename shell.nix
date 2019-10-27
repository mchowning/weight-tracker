let
  pkgs = import <nixpkgs> { };

  liquid =
    pkgs.runCommand "liquidhaskell" { buildInputs = [ pkgs.makeWrapper ]; } ''
      mkdir -p $out/bin
      ln -s ${pkgs.haskellPackages.liquidhaskell}/bin/liquid $out/bin
      wrapProgram $out/bin/liquid --prefix PATH : ${pkgs.z3}/bin
    '';

  ghc = pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
          vector
        ]);
in
  pkgs.stdenv.mkDerivation {
    name = "my-haskell-env-0";
    buildInputs = [ ghc liquid ];
    shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
  }