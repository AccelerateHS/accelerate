# Developing `accelerate` with nix

`accelerate` provides a nix flake which allows for simple development with `nix`. It builds on top of [haskell.nix](https://input-output-hk.github.io/haskell.nix/). 

## Provided outputs

- the `accelerate` flake outputs targets for a set of supported GHC versions. To see all provided outputs, run within the repo
  ```sh
  nix flake show --impure --allow-import-from-derivation
  ```
  This should output a tree similar to this one:
  ```
    ├───checks
    │   └───x86_64-linux
    │       ├───"ghc92:accelerate:test:doctest": derivation 'accelerate-test-doctest-1.3.0.0-check'
    │       ├───"ghc92:accelerate:test:nofib-interpreter": derivation 'accelerate-test-nofib-interpreter-1.3.0.0-check'
    │       ├───"ghc92:devShell": derivation 'ghc-shell-for-accelerate'
    │       ├───"ghc92:formatCheck": derivation 'pre-commit-run'
    │       ├───"ghc94:accelerate:test:doctest": derivation 'accelerate-test-doctest-1.3.0.0-check'
    │       ├───"ghc94:accelerate:test:nofib-interpreter": derivation 'accelerate-test-nofib-interpreter-1.3.0.0-check'
    │       ├───"ghc94:devShell": derivation 'ghc-shell-for-accelerate'
    │       ├───"ghc94:formatCheck": derivation 'pre-commit-run'
    │       ├───"ghc96:accelerate:test:doctest": derivation 'accelerate-test-doctest-1.3.0.0-check'
    │       ├───"ghc96:accelerate:test:nofib-interpreter": derivation 'accelerate-test-nofib-interpreter-1.3.0.0-check'
    │       ├───"ghc96:devShell": derivation 'ghc-shell-for-accelerate'
    │       └───"ghc96:formatCheck": derivation 'pre-commit-run'
    ├───devShells
    │   └───x86_64-linux
    │       ├───default: development environment 'nix-shell'
    │       ├───ghc92: development environment 'ghc-shell-for-accelerate'
    │       ├───ghc94: development environment 'ghc-shell-for-accelerate'
    │       ├───ghc96: development environment 'ghc-shell-for-accelerate'
    │       └───tooling: development environment 'nix-shell'
    ├───flakes: unknown
    ├───lib: unknown
    ├───packages
    │   └───x86_64-linux
    │       ├───"ghc92:accelerate:exe:tracy": package 'accelerate-exe-tracy-1.3.0.0'
    │       ├───"ghc92:accelerate:exe:tracy-capture": package 'accelerate-exe-tracy-capture-1.3.0.0'
    │       ├───"ghc92:accelerate:lib:accelerate": package 'accelerate-lib-accelerate-1.3.0.0'
    │       ├───"ghc92:accelerate:test:doctest": package 'accelerate-test-doctest-1.3.0.0'
    │       ├───"ghc92:accelerate:test:nofib-interpreter": package 'accelerate-test-nofib-interpreter-1.3.0.0'
    │       ├───"ghc94:accelerate:exe:tracy": package 'accelerate-exe-tracy-1.3.0.0'
    │       ├───"ghc94:accelerate:exe:tracy-capture": package 'accelerate-exe-tracy-capture-1.3.0.0'
    │       ├───"ghc94:accelerate:lib:accelerate": package 'accelerate-lib-accelerate-1.3.0.0'
    │       ├───"ghc94:accelerate:test:doctest": package 'accelerate-test-doctest-1.3.0.0'
    │       ├───"ghc94:accelerate:test:nofib-interpreter": package 'accelerate-test-nofib-interpreter-1.3.0.0'
    │       ├───"ghc96:accelerate:exe:tracy": package 'accelerate-exe-tracy-1.3.0.0'
    │       ├───"ghc96:accelerate:exe:tracy-capture": package 'accelerate-exe-tracy-capture-1.3.0.0'
    │       ├───"ghc96:accelerate:lib:accelerate": package 'accelerate-lib-accelerate-1.3.0.0'
    │       ├───"ghc96:accelerate:test:doctest": package 'accelerate-test-doctest-1.3.0.0'
    │       └───"ghc96:accelerate:test:nofib-interpreter": package 'accelerate-test-nofib-interpreter-1.3.0.0'
    ├───pkgs: unknown
    ├───projects: unknown
    ├───supportedghcs: unknown
    ├───supportedsystems: unknown
    └───toolingShellFor: unknown
  ```
  As you can see, for each of the supported GHC versions we generate a `devShell` in `devShells.<your-system>`, all the checks, i.e. the formatting checks as well as
  the Haskell tests, and, most importantly, the packages which contain the library, as well as the executables. 
- make sure you trust the substituters and pubkeys of this flake because otherwise you might have to bootstrap several `ghc`s *and* all of the dependencies

## Building your own version of `accelerate`

- make sure you have all files you want to be part of the build added to `git`
- run
  ```sh
  nix build .#<your-ghc-version>:<your-target`
  ```
  As an example, if you want to build the accelerate library for `ghc94`, run
  ```sh
  nix build .#ghc94:accelerate:lib:accelerate
  ```
  It is recommended to always pass `-Lv` to the nix command

## Provisioning a `devShell`

### Without `direnv`

- run
  ```sh
  nix develop .#<your-ghc-version>
  ```
  As an example, here's how you provide a `devShell` for `ghc92`:
  ```sh
  nix develop .#ghc92
  ```

### With direnv

- `direnv` provides an incredibly easy way of using `nix`, just put
  ```sh
  use flake .#<your-target> -Lv
  ```
  in a `.envrc` file and run
  ```sh
  direnv allow
  ```
- this has two major advantages
  - `direnv` automatically enters the `devShell` as soon as you enter the repo
  - `direnv` *caches* your shell, which means that as long as you change nothing about the `.cabal` file or the `nix` code, the startup of the
    `devShell` should be instant
- you can find documentation on how to install `direnv` [at their GitHub](https://github.com/direnv/direnv/)

### Using `pre-commit-hooks`

- the `devShell` automatically installs a `pre-commit` check for you, which you can use to
  - run formatting checks:
    ```sh
    pre-commit run -a
    ```
  - make sure to not forget to run formatting before `commit`ing
  - to surpass the `pre-commit`-check, add the `--no-verify` flag to your `git commit`-command

## Internals

- **the `--impure` trick:** nix has some problems running certain commands (`nix flake show` and `nix flake check`) with IFD if support
  for multiple systems is provided. We can trick nix into looking up our system by passing the `--impure` flag and use `builtins.currentSystem`
- **The `flattenAttrs` function from `nix/lib.nix`**:  the `nix` flake output schema doesn't use nested arguments, that's why we flatten the
  tree of outputs down to simple outputs by concatentating their path with `:`
- **Provisioning of `tracy`:** `accelerate` tries to normally provision `tracy` by itself, which is very much not how one normally `nix` does it with `nix`. That's why the
  `nix` build passes a flag to the build command instructing `Setup.hs` not to build it but instead get `tracy` as a `nix` derivation.
- Please contact [mangoiv](contact@mangoiv.com) if you have any questions with regards to the workings of the nix setup

### Updating dependencies
- the haskell dependencies are pinned via the `stack` snapshots, the available `stack` snapshots are provided via the `stackage.nix` input of the `haskell.nix` input,
  so if you want to use a `stack` snapshot which cannot be found within the `nix` setup, run
  ```sh
  nix flake lock --update-input haskell-nix
  ```
- to update all inputs, run
  ```sh
  nix flake update
  ```
- to update `tracy`, change the `rev` or `ref` in the input url of `inputs.tracy`
- to add a `ghc` version, add the `ghc` version in `supportedghcs` and either
  update the `stackYamlModifier` accordingly or add a corresponding `stack.yaml` in
  the root of the repo
