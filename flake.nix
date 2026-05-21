{
  description = "Wrapper around sbcl for fast script execution with cached ASDF system images";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hpkgs = pkgs.haskellPackages;

        sbcl-wrap = hpkgs.callCabal2nix "sbcl-wrap" ./. { };

        # sbcl with the libraries used in the test suite / README examples.
        # The wrapper prepends (require :asdf) to stdin when called with no args
        # (sbcl-wrap's image-building path), so asdf:load-system works without
        # a user's quicklisp in HOME.
        sbclWithTestLibs =
          let base = pkgs.sbcl.withPackages (ps: [ ps.alexandria ps.puri ]);
          in pkgs.writeShellScriptBin "sbcl" ''
            if [ $# -eq 0 ]; then
              # Image-build mode: sbcl-wrap pipes a Lisp script to stdin.
              # Prepend (require :asdf) so asdf:load-system is available.
              { printf '%s\n' "(require :asdf)"; cat; } | exec ${base}/bin/sbcl
            else
              exec ${base}/bin/sbcl "$@"
            fi
          '';
      in
      {
        packages = {
          default = pkgs.haskell.lib.justStaticExecutables sbcl-wrap;
          inherit sbcl-wrap;
        };

        # nix run → runs sbcl-wrap
        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/sbcl-wrap";
        };

        devShells.default = hpkgs.shellFor {
          packages = _: [ sbcl-wrap ];
          buildInputs = [
            pkgs.cabal-install
            pkgs.stack
            hpkgs.haskell-language-server
            pkgs.sbcl
          ];
          withHoogle = true;
        };

        # nix run .#test — end-to-end integration test
        apps.test = {
          type = "app";
          # Reference ./test as a directory so all .lisp files land in the same store path.
          program = toString (pkgs.writeShellScript "sbcl-wrap-tests" ''
            export PATH="${self.packages.${system}.default}/bin:${sbclWithTestLibs}/bin:$PATH"
            exec bash ${./test}/run-tests.sh
          '');
        };
      }
    );
}
