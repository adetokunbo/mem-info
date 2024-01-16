let
  pkgs = import ./h8x.nix;
  pkgsMusl64 = pkgs.pkgsCross.musl64;
  mkApp = import ./default.nix;
  appMusl64 = mkApp { pkgs = pkgsMusl64; };

in {
  staticPrintmem = appMusl64.mem-info.components.exes.printmem // {
    configureFlags = pkgsMusl64.lib.optionals pkgsMusl64.stdenv.targetPlatform.isMusl [
      "--disable-executable-dynamic"
      "--disable-shared"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-L${pkgsMusl64.gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${pkgsMusl64.zlib}/lib"
    ];
  };
}
