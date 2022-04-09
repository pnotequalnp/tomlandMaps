{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = final.lib.composeExtensions (prev.haskell.packageOverrides)
            (hsFinal: hsPrev: {
              tomlandMaps = hsFinal.callCabal2nix "tomlandMaps" ./. { };
            });
        };
      };
    in
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ overlay ];
          };
          hs = pkgs.haskellPackages;
        in
        rec {
          packages = rec {
            tomlandMaps = hs.tomlandMaps;
            default = tomlandMaps;
          };

          apps = rec {
            tomlandMaps = flake-utils.lib.mkApp { drv = packages.tomlandMaps; };
            default = tomlandMaps;
          };

          devShells.default = hs.shellFor {
            packages = hsPkgs: with hsPkgs; [ tomlandMaps ];
            nativeBuildInputs = with hs; [
              cabal-install
              fourmolu
              haskell-language-server
              hlint
            ];
          };
        }) // {
      overlays.default = overlay;
    };
}
