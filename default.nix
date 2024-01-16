let
  pkgsNix = import ./h8x.nix;
in
{ pkgs ? pkgsNix
} : pkgs.haskell-nix.cabalProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "mem-info";
    src = ./.;
  };

  # Specify the GHC version to use.
  compiler-nix-name = "ghc928";

  # Specify the hackage index state
  index-state = "2023-07-20T00:00:00Z";
}
