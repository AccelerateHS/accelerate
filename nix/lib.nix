{lib, ...}:
with lib; rec {
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

  # utility function that generates flakes and flake outputs based on the ghcVersions
  # you pass it, it receives them in the format [[8 10 7] [9 0 2]]
  # hh provides haskell.nix project and flake, as well as the standard flake outputs
  # packages, checks and devShells
  mkFlakeAttrsFor = {
    ghcversions,
    projFun,
    sysFun,
    precommitcheckForghc,
  }: let
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
          // {formatCheck = precommitcheckForghc sys;};
      })
      fs;
  in {
    projects = builtins.listToAttrs ps;
    flakes = builtins.listToAttrs fs;
    devShells = sysFun (sys: flattenAttrs ":" (builtins.listToAttrs (mkOutput "devShell" sys)));
    packages = sysFun (sys: flattenAttrs ":" (builtins.listToAttrs (mkOutput "packages" sys)));
    checks = sysFun (sys: flattenAttrs ":" (builtins.listToAttrs (mkChecks sys)));
  };
}
