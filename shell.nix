{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, alex, array, base, containers, data-default
      , directory, happy, lens, mtl, parsec, stdenv, utf8-string, cabal-install
      }:
      mkDerivation {
        pname = "angler";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          array base containers data-default directory lens mtl parsec
          utf8-string
        ];
        executableToolDepends = [ cabal-install alex happy ];
        homepage = "https://github.com/angler-lang";
        description = "Angler Functional Programming Language";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
