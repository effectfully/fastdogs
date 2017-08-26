{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, containers, directory
      , filepath, optparse-applicative, process, stdenv, text,
      cabal-install, hasktags
      }:
      mkDerivation {
        pname = "haskdogs";
        version = "0.4.5";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base bytestring containers directory filepath optparse-applicative
          process text hasktags
        ];
        libraryHaskellDepends = [
          cabal-install
        ];
        homepage = "http://github.com/grwlf/haskdogs";
        description = "Generate tags file for Haskell project and its nearest deps";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
