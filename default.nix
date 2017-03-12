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
      justCabalResult = s: reflex.cabal2nixResult (justCabal s);
      local = s: hlib.overrideCabal (self.callPackage (justCabalResult s) {}) (drv: {
        src = builtins.filterSource (path: type:
          type != "unknown"
          && baseNameOf path != ".git"
          && baseNameOf path != "result"
          && baseNameOf path != "dist"
          && baseNameOf path != ".stack-work") s;
      });
    in {
      callHackage = import ./callHackageFix.nix pkgs self;
  
      reflex-dom-contrib = self.callPackage (pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-dom-contrib";
        rev = "df4138406a5489acd72cf6c9e88988f13da02b31";
        sha256 = "051x79afwzfbjv38z348pysnz49nmqg1czywd2s1sigvsdkg0gp9";
      }) {};

      http-api-data = hlib.dontCheck (self.callHackage "http-api-data" "0.3.5" {});
      natural-transformation = self.callHackage "natural-transformation" "0.4" {};

      servant = self.callHackage "servant" "0.10" {};
      servant-server = hlib.dontCheck (self.callHackage "servant-server" "0.10" {});
      servant-blaze = self.callHackage "servant-blaze" "0.7.1" {};

      servant-router = local ./servant-router;
      server-example = local ./examples/server;
      reflex-example = local ./examples/reflex;
    };
}
