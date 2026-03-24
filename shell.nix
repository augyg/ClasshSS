{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  hpkgs = haskellPackages.override {
    overrides = self: super: {
      henforcer = self.callHackage "henforcer" "1.0.0.1" {};
    };
  };
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  classhSS = import ./default.nix;
  drv = variant (hpkgs.callPackage classhSS {});
in
pkgs.mkShell {
  buildInputs = [ pkgs.cabal-install ];
  inputsFrom = [ (if pkgs.lib.inNixShell then drv.env else drv) ];
}
