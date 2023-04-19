{src}: {
  inherit src;
  settings = {
    ormolu.defaultExtensions = [];
  };

  hooks = {
    cabal-fmt.enable = false;
    fourmolu.enable = false;
    hlint.enable = false;

    alejandra.enable = true;
    statix.enable = true;
    deadnix.enable = true;
  };
}
