{
  description = "Accelerate - High-performance parallel arrays for Haskell";
  nixConfig = {
    bash-prompt = "\\e[34;1maccelerate-devShell ~ \\e[0m";

    allow-import-from-derivation = true;

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

      # We support a bunch of systems and ghc versions,
      # this is what the flakes provides outputs for
      # If you want to run nix flake show, make sure this is a singleton list that only contains your system,
      # because IFD will not succeed to build for other specified systems
      supportedsystems = ["x86_64-linux"];
      # supportedsystems = systems.flakeExposed;

      # We cannot easily support ghc865 with nix as it's so much out of date
      # that it's not included in nixpkgs anymore
      supportedghcs = [
        [8 10 7]
        [9 0 2]
        [9 2 4]
        /*
        [9 4 2] FIXME: we're waiting for stackage nightly to upgrade to 942 and haskell.nix getting compiler support
                       for it
        */
      ];

      perSystem = genAttrs supportedsystems;

      # Utility function that provides a bunch of useful
      # representations of ghc versions, receives a list with
      # three elements that represent a ghc version, e.g. [9 2 4]
      ghcVer = l: let
        allstr = builtins.map builtins.toString;
        shortRaw = take 2 (allstr l);
      in rec {
        long = concatStrings (allstr l);
        short = concatStrings shortRaw;
        stack = concatStringsSep "." shortRaw;
        compiler-nix-name = "ghc${long}";
      };

      # Utility function that will take a separator and an attrset and
      # flatten the attrset until only it's leaves remain, the keys for the
      # new attrset are sep-separated old keys
      flattenAttrs = sep: attrs: let
        recurse = p:
          mapAttrsToList
          (n: v: let
            p' =
              if p == ""
              then p
              else p + sep;
          in
            if (isAttrs v && !(isDerivation v))
            then recurse (p' + n) v
            else {${p' + n} = v;});
      in
        foldr (a: b: a // b) {} (flatten (recurse "" attrs));

      # utility functions that, passed a ghc version in the list format
      # and a system name returns hls, hlint and fourmolu
      toolsForghc = ghcversion: system: let
        gver = ghcVer ghcversion;
      in
        {
          haskell-language-server = (plainpkgsFor system).haskell-language-server.override {supportedGhcVersions = [gver.long];};
          inherit ((plainpkgsFor system).haskell.packages.${gver.compiler-nix-name}) hlint fourmolu;
        }
        //
        # ghc 8 doesn't build fourmolu 0.4 because it doesn't provide the correct cabal version
        (
          if (builtins.head ghcversion) == 8
          then {
            inherit ((plainpkgsFor system).haskellPackages) fourmolu;
          }
          else {}
        );

      # utility function that, passed a ghc version in the list format
      # and a system name returns a pre-commit-check attrset with a shellHook
      # and a formatCheck that can be run
      precommitcheckForghc = ghcversion: system:
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

          tools = {inherit (toolsForghc ghcversion system) fourmolu hlint;};
        };

      # builds, given a ghc version in the list format and a system name, a
      # haskell.nix stackProject that contains all accelerate libraries and executables
      # the resolver is chosen based on the ghcVersion passed, i.e. if you want a specific
      # ghc version, you need the appropriate resolver
      projectForghc = ghcversion: system: let
        pkgs = pkgsFor system;
        plainpkgs = plainpkgsFor system;
        gver = ghcVer ghcversion;
        tools = toolsForghc ghcversion system;
      in
        pkgs.haskell-nix.stackProject' {
          src = ./.;
          inherit (gver) compiler-nix-name;
          stackYaml = "stack-${gver.stack}.yaml";
          shell = {
            inherit (precommitcheckForghc ghcversion system) shellHook;
            withHoogle = true;
            exactDeps = true;

            nativeBuildInputs = [
              plainpkgs.alejandra
              plainpkgs.cabal-install
              plainpkgs.stack
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

      # a tooling shell that provides all the necessary stuff to do
      # formatting and quick adjustments, it does not provide a full dev env
      toolingShellFor = ghcversion: system: let
        plainpkgs = plainpkgsFor system;
        tools = toolsForghc ghcversion system;
      in
        plainpkgs.mkShell {
          inherit (precommitcheckForghc ghcversion system) shellHook;
          nativeBuildInputs = [
            plainpkgs.cabal-install
            plainpkgs.stack
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

      # utility function that generates flakes and flake outputs based on the ghcVersions
      # you pass it, it receives them in the format [[8 10 7] [9 0 2]]
      # it provides haskell.nix project and flake, as well as the standard flake outputs
      # packages, checks and devShells
      mkFlakeAttrsFor = ghcversions: projFun: sysFun: let
        ps =
          map (
            ver: let
              gver = ghcVer ver;
            in {
              name = "project-${gver.short}";
              value = sysFun (projFun ver);
              origVer = ver;
            }
          )
          ghcversions;

        fs =
          map (p: let
            gver = ghcVer p.origVer;
          in {
            inherit (p) origVer;
            name = "flake-${gver.short}";
            value = sysFun (sys: p.value.${sys}.flake {});
          })
          ps;

        mkOutput = attrName: sys:
          map (f: {
            name = "ghc${(ghcVer f.origVer).short}";
            value = f.value.${sys}.${attrName};
          })
          fs;

        mkChecks = sys:
          map (f: {
            name = "ghc${(ghcVer f.origVer).short}";
            value =
              f.value.${sys}.checks
              // {formatCheck = precommitcheckForghc f.origVer sys;};
          })
          fs;
      in {
        projects = builtins.listToAttrs ps;
        flakes = builtins.listToAttrs fs;
        devShells = sysFun (sys: flattenAttrs "-" (builtins.listToAttrs (mkOutput "devShell" sys)));
        packages = sysFun (sys: flattenAttrs "-" (builtins.listToAttrs (mkOutput "packages" sys)));
        checks = sysFun (sys: flattenAttrs "-" (builtins.listToAttrs (mkChecks sys)));
      };

      accelerateFlakes = mkFlakeAttrsFor supportedghcs projectForghc perSystem;
    in {
      inherit supportedsystems supportedghcs flattenAttrs ghcVer mkFlakeAttrsFor toolingShellFor toolsForghc;
      # FIXME: checks have to be fixed; checks pass with stack --nix test but not with cabal test
      inherit (accelerateFlakes) flakes projects packages checks;

      pkgs = perSystem pkgsFor;
      plainpkgs = perSystem plainpkgsFor;

      devShells = perSystem (sys:
        accelerateFlakes.devShells.${sys}
        // rec {
          # the default shell is the tooling shell as it loads fastest
          default = tooling;
          tooling = toolingShellFor [9 2 4] sys;
        });

      formatter = perSystem (system: self.pkgs.${system}.alejandra);
    };
}
