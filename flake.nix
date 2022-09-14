{
  description = "Accelerate - High-performance parallel arrays for Haskell";
  nixConfig = {
    bash-prompt = "accelerate-devShell ~ ";

    substituters = [
      "https://cache.nixos.org" # nixos cache
      "https://hydra.iohk.io" # iog hydra cache
      "https://iohk.cachix.org" # iog cachix cache
    ];

    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" # nixos pubkey
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" # iog hydra pubkey
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=" # iog cachix pubkey
    ];
  };

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    nixpkgs-upstream.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs-upstream";
    };
  };

  outputs = {
    self,
    haskell-nix,
    nixpkgs,
    nixpkgs-upstream,
    pre-commit-hooks,
  }:
    with nixpkgs.lib; let
      pkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [haskell-nix.overlay];
        };

      # the reason why we have a plainPkgs is that the nixpkgs version used by IOG's haskell.nix is sometimes out of date
      # and the caching of tools is worse than on nixpkgs due to the use of their haskell-nix overlay
      plainpkgsFor = system: import nixpkgs-upstream {inherit system;};

      supportedsystems = systems.flakeExposed;
      perSystem = genAttrs supportedsystems;

      toolsForGHC = ghcversion: system: {
        haskell-language-server = (plainpkgsFor system).haskell-language-server.override {supportedGhcVersions = [ghcversion];};
        hlint = (plainpkgsFor system).haskell.packages."ghc${ghcversion}".hlint_3_4_1;
        fourmolu = with nixpkgs.lib;
        # use fourmolu 0.6 for ghc9+
          if (head (stringToCharacters ghcversion) == "9")
          then (plainpkgsFor system).haskell.packages."ghc${ghcversion}".fourmolu_0_6_0_0
          else (plainpkgsFor system).haskellPackages.fourmolu;
      };

      precommitcheckForGHC = ghcversion: system:
        pre-commit-hooks.lib.${system}.run
        {
          src = ./.;
          settings = {
            ormolu.defaultExtensions = [];
          };

          hooks = {
            cabal-fmt.enable = true;
            fourmolu.enable = false;
            hlint.enable = false;
            alejandra.enable = true;
            statix.enable = true;
            shellcheck.enable = true;
          };

          tools = {inherit (toolsForGHC ghcversion system) fourmolu hlint;};
        };

      projectForGHC = ghcversion: system: let
        pkgs = pkgsFor system;
        plainpkgs = plainpkgsFor system;
        tools = toolsForGHC ghcversion system;
        compiler-nix-name = "ghc${ghcversion}";
      in
        pkgs.haskell-nix.project {
          src = ./.;
          inherit compiler-nix-name;
          shell = {
            inherit (precommitcheckForGHC ghcversion system) shellHook;
            withHoogle = true;
            exactDeps = true;

            nativeBuildInputs = [
              plainpkgs.alejandra
              plainpkgs.cabal-install
              plainpkgs.fd
              plainpkgs.ripgrep

              plainpkgs.haskellPackages.apply-refact
              plainpkgs.haskellPackages.cabal-fmt

              tools.fourmolu
              tools.haskell-language-server
              tools.hlint
            ];
          };
        };

      toolingShellFor = ghcVersion: system: let
        plainpkgs = plainpkgsFor system;
        tools = toolsForGHC ghcVersion system;
      in
        plainpkgs.mkShell {
          inherit (precommitcheckForGHC ghcVersion system) shellHook;
          nativeBuildInputs = [
            plainpkgs.cabal-install
            plainpkgs.fd
            plainpkgs.ripgrep

            plainpkgs.alejandra
            plainpkgs.haskellPackages.apply-refact
            plainpkgs.haskellPackages.cabal-fmt

            tools.fourmolu
            tools.haskell-language-server
            tools.hlint
          ];
        };

      project-94 = perSystem (projectForGHC "942");
      project-92 = perSystem (projectForGHC "924");
      project-810 = perSystem (projectForGHC "8107");
    in {
      inherit cabalprojectlocal project-810 project-92 project-94;
      pkgs = perSystem pkgsFor;
      plainpkgs = perSystem plainpkgsFor;
      flake-810 = perSystem (system: self.project-810.${system}.flake {});
      flake-92 = perSystem (system: self.project-92.${system}.flake {});
      flake-94 = perSystem (system: self.project-94.${system}.flake {});

      packages = perSystem (system: {
        ghc94 = self.flake-94.${system}.packages;
        ghc92 = self.flake-92.${system}.packages;
        ghc810 = self.flake-810.${system}.packages;
      });

      devShells = perSystem (system: rec {
        tooling = toolingShellFor "924" system;
        ghc94 = self.flake-94.${system}.devShell;
        ghc92 = self.flake-92.${system}.devShell;
        ghc810 = self.flake-810.${system}.devShell;
        default = tooling;
      });

      formatter = perSystem (system: self.pkgs.${system}.alejandra);
    };
}
