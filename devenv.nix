{ pkgs, ... }:
{
  # CL + Haskell tooling for working on sbcl-wrap
  packages = with pkgs; [
    sbcl
    cabal-install
    stack
    rlwrap   # nicer sbcl REPL
  ];

  languages.haskell.enable = true;

  enterShell = ''
    echo "sbcl-wrap dev shell"
    echo "  ghc:  $(ghc --version)"
    echo "  sbcl: $(sbcl --version)"
    echo ""
    echo "  stack build        – compile"
    echo "  stack install      – install to ~/.local/bin"
    echo "  make nix-install   – install via Nix profile"
    echo "  nix run .#test     – run integration tests"
  '';
}
