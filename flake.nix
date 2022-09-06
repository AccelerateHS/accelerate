{
  description = "Accelerate - High-performance parallel arrays for Haskell";
  nixConfig.bash-prompt = "accelerate-devShell ~ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }:
    with nixpkgs.lib; let
      pkgsFor = system: import nixpkgs {inherit system;};

      supportedSystems = systems.flakeExposed;
      perSystem = genAttrs supportedSystems;

      devShellFor = system: ghcVersion: let
        pkgs = pkgsFor system;
        basePkgs = ps: with ps; [
          Cabal
          base
          base
          base-orphans
          bytestring
          cabal-doctest
          containers
          deepseq
          directory
          directory
          doctest
          double-conversion
          exceptions
          filepath
          filepath
          formatting
          ghc-prim
          half
          hashable
          hashtables
          hedgehog
          microlens
          mtl
          prettyprinter
          prettyprinter-ansi-terminal
          primitive
          tasty
          template-haskell
          terminal-size
          text
          transformers
          unique
          unordered-containers
          vector
        ];

        testPkgs = ps: with ps; [
          ansi-terminal
          tasty-expected-failure
          tasty-hedgehog
          tasty-hunit
          tasty-rerun
        ];

        hsPkgs = ps: (testPkgs ps) ++ (basePkgs ps);
        compiler-nix-name = "ghc" + ghcVersion;
        ghcForAccelerate = pkgs.haskell.packages.${compiler-nix-name}.ghcWithPackages hsPkgs;
        hls = pkgs.haskell-language-server.override {supportedGhcVersions = [ghcVersion];};
      in
        pkgs.mkShell {
          buildInputs = [
            ghcForAccelerate
            hls
            pkgs.cabal-install
            pkgs.haskellPackages.cabal-fmt
            pkgs.haskellPackages.ansi-terminal
            pkgs.haskell.packages.${compiler-nix-name}.fourmolu_0_6_0_0
            pkgs.hlint
          ];
        };
    in {
      pkgs = perSystem pkgsFor;

      devShells = perSystem (system: rec {
        ghc924 = devShellFor system "924";
        default = ghc924;
      });

      formatter = perSystem (system: self.pkgs.${system}.alejandra);
    };
}
