{
  description = "ekg-core";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils/master";
  };
  outputs = { self, nixpkgs, flake-utils }:
    with flake-utils.lib;
    with nixpkgs.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let version = "${substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
          overlay = oself: osuper:
            with oself;
            with haskell.lib;
            with haskellPackages;
            {
              ekg-core = overrideCabal (dontCheck (callCabal2nix "ekg-core" ./. {})) (o: { version = "${o.version}-${version}"; });
            };
          overlays = [ overlay ];
      in
        with (import nixpkgs { inherit system overlays; });
        rec {
          packages = flattenTree (recurseIntoAttrs { inherit ekg-core; });
          defaultPackage = packages."ekg-core";
        });
}
