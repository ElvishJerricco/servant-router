# Reflex platform is using a very outdated nixpkgs, and updating would
# be pretty nontrivial. To use `callHackage` to bump packages to newer
# versions, we have to update `all-cabal-hashes`. I tried doing this
# in the `packageOverrides` to `nixpkgsFunc` in `reflex-platform`, but
# `haskellPackages` didn't seem to notice the difference, and used the
# old `all-cabal-hashes` anyway. So here we just redefine
# `callHackage` to use our own.

pkgs: haskellPackages:

let
  all-cabal-hashes = pkgs.fetchFromGitHub {
    owner = "commercialhaskell";
    repo = "all-cabal-hashes";
    rev = "a60545d2dc8177b1de1629e1f9119235822d5b83";
    sha256 = "12szn4fckrxgzxdvlgks536lmxkahqmmnikla4vi5mlc494bbvml";
  };

  haskellSrc2nix = { name, src, sha256 ? null }:
    let
      sha256Arg = if isNull sha256 then "--sha256=" else ''--sha256="${sha256}"'';
    in pkgs.stdenv.mkDerivation {
      name = "cabal2nix-${name}";
      buildInputs = [ pkgs.cabal2nix ];
      phases = ["installPhase"];
      LANG = "en_US.UTF-8";
      LOCALE_ARCHIVE = pkgs.lib.optionalString pkgs.stdenv.isLinux "${pkgs.glibcLocales}/lib/locale/locale-archive";
      installPhase = ''
        export HOME="$TMP"
        mkdir -p "$out"
        cabal2nix --compiler=${haskellPackages.ghc.name} --system=${pkgs.stdenv.system} ${sha256Arg} "${src}" > "$out/default.nix"
      '';
    };

  hackage2nix = name: version: haskellSrc2nix {
    name   = "${name}-${version}";
    sha256 = ''$(sed -e 's/.*"SHA256":"//' -e 's/".*$//' "${all-cabal-hashes}/${name}/${version}/${name}.json")'';
    src    = "${all-cabal-hashes}/${name}/${version}/${name}.cabal";
  };

in name: version: haskellPackages.callPackage (hackage2nix name version)
