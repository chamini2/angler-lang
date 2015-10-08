{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, alex, array, base, containers, data-default
      , directory, filepath, happy, lens, megaparsec, mtl, stdenv
      , utf8-string
      }:
      mkDerivation {
        pname = "angler";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          array base containers data-default directory filepath lens
          megaparsec mtl utf8-string
        ];
        executableToolDepends = [ alex happy ];
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
