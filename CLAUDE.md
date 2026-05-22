# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

`sbcl-wrap` is a Haskell CLI tool that wraps SBCL (Steel Bank Common Lisp) to enable fast Common Lisp script execution. It pre-compiles a set of ASDF systems into a cached SBCL core image (`~/.cache/sbcl-wrap/<md5-of-system-names>.core`), then re-uses that image on subsequent runs — turning a multi-second cold start into ~50 ms.

Scripts invoke it via shebang: `#!/usr/local/bin/sbcl-wrap alexandria puri --`

## Build & install

**Nix (preferred — no Haskell toolchain needed on the host):**

```bash
nix build                  # build → ./result/bin/sbcl-wrap
make nix-install           # nix profile install . → installs to Nix profile
nix run .#test             # run integration test suite
nix develop                # dev shell: GHC, cabal, HLS, sbcl
```

## Architecture (single file: `src/Main.hs`)

The entire implementation is one file with these logical layers:

1. **Argument parsing** (`parseArgs`) — splits CLI args on `--`, yielding `(systemNames, sbclScript, sbclArgs)`. The first argument may be space-separated (shebang passes everything as one string), handled by `splitFirst`.

2. **Cache management** (`ensureImage`) — hashes the sorted, lowercased system names with MD5 (`systemsHash`) to produce a cache key. Calls `makeImage` on a miss.

3. **Image building** (`makeImage` / `sbclScript`) — spawns `sbcl` with no args, pipes a Lisp script that `(asdf:load-system ...)` each system and calls `sb-ext:save-lisp-and-die`. stdout/stderr suppressed during build.

4. **Execution** (`execSbcl`) — `executeFile` (POSIX exec, not a subprocess) into `sbcl --core <image>` via `sb-impl::process-script`. Sets `SBCL_WRAP_SCRIPT_NAME` env var. Passes `--dynamic-space-size 2048`, disables sysinit/userinit.

5. **Error handling** — hand-rolled `EitherT (String, Int) IO` monad. Exit codes: 88 = bad args, 77 = image build failure.

## Test suite

```bash
nix run .#test   # runs test/run-tests.sh end-to-end
```

Tests: exit-88 smoke, bare-sbcl round-trip (cold + cached), alexandria+puri round-trip (cold + cached).

## Nix implementation notes

- `flake.nix` builds the package via `haskellPackages.callCabal2nix`.
- `nix run .#test` uses `sbclWithTestLibs`: a `sbcl.withPackages [alexandria puri]` binary wrapped in a shell script that prepends `(require :asdf)` to stdin when called with no args. This is necessary because nixpkgs's `sbcl.withPackages` does not retain the ASDF package in the saved image, so `makeImage`'s piped script would otherwise fail with "Package ASDF does not exist."
- `devenv.nix` provides a devenv.sh-compatible shell alongside the flake `devShell`.
