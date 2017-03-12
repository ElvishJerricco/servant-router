# This file overrides Reflex's haskellPackages, adding all the packages we need.
# This includes bumping some dependencies.
# Import this to get a haskellPackages with all the targets in this project.

{ reflex ? import ./reflex-platform.nix {}
, env ? "ghc"
, ghc ? null # Stack gives us this. We don't care
}:

reflex.${env}.override {
  overrides = self: super:
    let
      pkgs = reflex.nixpkgs;
      hlib = pkgs.haskell.lib;
      justCabal = builtins.filterSource (path: type: pkgs.lib.hasSuffix ".cabal" path);
      # `cabal2NixResult` copies your entire source directory to get
      # the cabal file. This is undesirable because it means
      # rebuilding that derivation whenever anything changes, rather
      # than just when the cabal file changes. In Nix, this isn't
      # usually a problem, even if it pollutes your nix store a
      # bit. But Intero freaks out when it sees the unexpected
      # "building cabal2nixResult" output. This function isolates just
      # the cabal files in the source directory.
      justCabalResult = s: reflex.cabal2nixResult (justCabal s);
      # After isolating the cabal file, override the resulting
      # derivation to set the `src` to the local source directory, but
      # also filter out unwanted paths.
      local = s: hlib.overrideCabal (self.callPackage (justCabalResult s) {}) (drv: {
        src = builtins.filterSource (path: type:
          type != "unknown"
          && baseNameOf path != ".git"
          && baseNameOf path != "result"
          && baseNameOf path != "dist"
          && baseNameOf path != ".stack-work") s;
      });
    in {
      # Fix callHackage's out of date `all-cabal-hashes`.
      callHackage = import ./callHackageFix.nix pkgs self;

      # Bump `reflex-dom-contrib` from GitHub.  
      reflex-dom-contrib = self.callPackage (pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-dom-contrib";
        rev = "df4138406a5489acd72cf6c9e88988f13da02b31";
        sha256 = "051x79afwzfbjv38z348pysnz49nmqg1czywd2s1sigvsdkg0gp9";
      }) {};

      # Bump dependencies.
      http-api-data = hlib.dontCheck (self.callHackage "http-api-data" "0.3.5" {});
      natural-transformation = self.callHackage "natural-transformation" "0.4" {};

      servant = self.callHackage "servant" "0.10" {};
      servant-server = hlib.dontCheck (self.callHackage "servant-server" "0.10" {});
      servant-blaze = self.callHackage "servant-blaze" "0.7.1" {};

      # Set the locally defined packages.
      servant-router = local ./servant-router;
      server-example = local ./examples/server;
      reflex-example = local ./examples/reflex;
    };
}
