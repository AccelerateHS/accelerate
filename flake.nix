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
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    # non-nix inputs
    tracy.url = "github:wolfpld/tracy/v0.9.1";
    tracy.flake = false;
  };

  outputs = inputs @ {
    haskell-nix,
    nixpkgs,
    pre-commit-hooks,
    ...
  }: let
    pkgsFor = system:
      nixpkgs.legacyPackages.${system}.appendOverlays [
        haskell-nix.overlay
        # always build with the same tracy hat we provision to accelerate itself
        (_self: super: {
          tracy =
            inputs.nixpkgs-unstable.legacyPackages.${super.system}.tracy.overrideAttrs (_old: {src = inputs.tracy;});
        })
      ];

    # a bit hacky solution to get around nix flake show and nix flake check
    # not working with IFD, you have to pass --impure for it to
    # pick up your local system and then the checks (as well as
    # the show) will go through just fine
    supportedsystems =
      if builtins.hasAttr "currentSystem" builtins
      then [builtins.currentSystem]
      else nixpkgs.lib.systems.flakeExposed;

    perSystem = nixpkgs.lib.genAttrs supportedsystems;

    src = ./.;

    # the ghc verions supported as shown with the stack files
    supportedghcs = [
      [9 2 7]
      [9 4 4]
      [9 6 1]
    ];

    # what stack yaml to use if the ghc version
    # used doesn't have a stack file
    stackYamlModifier = ver:
      if ver == [9 6 1]
      then [9 4 4]
      else ver;

    lib = nixpkgs.lib // (import ./nix/lib.nix {inherit (nixpkgs) lib;});

    tools = {
      haskell-language-server = "1.10.0.0";
    };

    additionalBuildDepends = p:
      with p; [
        brotli
        bzip2
        capstone
        freetype
        glfw
        gtk3
        libffi
        libglvnd
        libpng
        libxkbcommon
        pkgconfig
        rt
        systemd
        tracy
        tbb
      ];

    # utility function that, passed a system name returns a pre-commit-check attrset
    # with a shellHook and a formatCheck that can be run
    precommitcheckForghc = system:
      pre-commit-hooks.lib.${system}.run (import ./nix/pre-commit-config.nix {inherit src;});

    # builds, given a ghc version in the list format and a system name, a
    # haskell.nix stackProject that contains all accelerate libraries and executables
    # the resolver is chosen based on the ghcVersion passed, i.e. if you want a specific
    # ghc version, you need the appropriate resolver
    projectForghc = ghcversion: system: let
      pkgs = pkgsFor system;
      gver = lib.ghcVer ghcversion;
      patch = ''
        flags:
          accelerate:
            nofib: true
            debug: true
            provisioned_tracy: true
      '';
      patchedSrc = pkgs.runCommand "patchStackYaml" {src = ./.;} ''
        mkdir $out
        cp -r $src/* $out
        chmod -R +w $out/cbits
        mkdir -p $out/cbits/tracy
        cp -rf ${inputs.tracy}/* $out/cbits/tracy/
        cd $out
        for stackYaml in stack-*.yaml;
        do
          echo "modifying stack file:"
          echo $stackYaml
          chmod +w $stackYaml
          echo "${patch}" >> $stackYaml
        done
      '';
    in
      pkgs.haskell-nix.stackProject' {
        src = patchedSrc;
        inherit (gver) compiler-nix-name;
        stackYaml = "stack-${(lib.ghcVer (stackYamlModifier ghcversion)).stack}.yaml";
        modules = [
          (_: {
            setup-depends = additionalBuildDepends pkgs;
            packages."accelerate" = {
              components.library.build-tools = additionalBuildDepends pkgs;
              components.tests."nofib-interpreter" = {
                build-tools = additionalBuildDepends pkgs;
              };
              components.tests."doctest" = {
                build-tools = additionalBuildDepends pkgs;
                enableShared = true;
              };
            };
          })
        ];
        shell = {
          inherit tools;
          inherit (precommitcheckForghc system) shellHook;
          inputsFrom = [(toolingShellFor system)];
          withHoogle = false;
          exactDeps = true;
        };
      };

    # a tooling shell that provides all the necessary stuff to do
    # formatting and quick adjustments, it does not provide a full dev env
    toolingShellFor = system: let
      pkgs = pkgsFor system;
    in
      pkgs.mkShell {
        inherit (precommitcheckForghc system) shellHook;
        buildInputs = []; # additionalBuildDepends pkgs;
        nativeBuildInputs = [
          pkgs.cabal-install
          pkgs.stack
        ];
      };

    accelerateFlakes = lib.mkFlakeAttrsFor {
      inherit precommitcheckForghc;
      ghcversions = supportedghcs;
      projFun = projectForghc;
      sysFun = perSystem;
    };
  in {
    inherit supportedsystems supportedghcs toolingShellFor lib;
    inherit (accelerateFlakes) flakes projects packages;

    pkgs = perSystem pkgsFor;

    # FIXME: checks have to be fixed; checks pass with stack --nix test but not with cabal test
    checks = perSystem (system: accelerateFlakes.checks.${system} // (lib.extendAttrName ":devShell" accelerateFlakes.devShells.${system}));

    devShells = perSystem (system:
      accelerateFlakes.devShells.${system}
      // rec {
        # the default shell is the tooling shell as it loads fastest
        default = tooling;
        tooling = toolingShellFor system;
      });
  };
}
