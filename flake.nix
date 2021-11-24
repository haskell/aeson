{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = aeson-dev.envFunc { withHoogle = true; };
            defaultPackage = aeson;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          aeson = hpkgs.callCabal2nix "aeson" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit aeson;
            aeson-dev = addBuildTools aeson [
              haskell-language-server
              cabal-install
            ];
          };
    };
}
