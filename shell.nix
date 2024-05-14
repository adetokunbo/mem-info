let
  project = import ./default.nix {};
in
  project.shellFor {
    # Don't build haddock to optimize build time
    withHaddock = false;
    withHoogle = false;

    # Some common tools can be added with the `tools` argument
    tools = {
      cabal = "latest";
      hlint = "latest"; # Selects the latest version in the hackage.nix snapshot
      haskell-language-server = "latest";
      ghcid = "latest";
      fourmolu = "0.14.0.0";
      cabal-fmt = "latest";
    };
    # See overlays/tools.nix for more details

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
