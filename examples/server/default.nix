{ reflex ? import ./reflex-platform.nix {}
, env ? "ghc"
, ghc ? null # Stack gives us this. We don't care
} @ args:

(import ../../. args).server-example
