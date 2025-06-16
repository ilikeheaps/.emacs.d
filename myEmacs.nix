{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:

let
  myEmacs = pkgs.emacs30.override {
    withGTK3 = false;
  };
  emacsWithPackages = (pkgs.emacsPackagesFor myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
      ansible
      elm-mode
      fill-column-indicator
      flycheck
      flycheck-haskell
      flycheck-ocaml
      flycheck-rust
      flycheck-status-emoji
      fsharp-mode
      haskell-emacs
      haskell-mode
      helm
      htmlize
      lispy
      lsp-mode
      lua-mode
      org-bullets
      org-tree-slide
      outshine
      plantuml-mode
      rainbow-delimiters
      rust-playground
      sbt-mode
      scala-mode
      sublimity
      tuareg
      wgrep-helm
      which-key
      yaml-mode
      # stuff for git-annex (managing files spread over multiple locations incl. backups)
      git-annex
      magit-annex
    ]) ++ (with epkgs.melpaPackages; [
      centered-window
      csharp-mode
      # ^ stable melpa version outputs error: https://github.com/emacs-csharp/csharp-mode/issues/208
      dired-narrow
      proof-general
      # ^ stable melpa version just didn't work
      lsp-haskell
      magit
      # ^ stable melpa is 2.9 with bug: https://github.com/magit/magit/issues/3843
      nix-mode
      # ^ stable version isn't updated for Emacs 28.x: https://github.com/doomemacs/doomemacs/issues/4534
      flycheck-plantuml
      forth-mode
      racket-mode
    ]) ++ (with epkgs.elpaPackages; [
      adaptive-wrap
      company
      csv-mode
      ediprolog
      rainbow-mode
      sml-mode
      undo-tree
    ])
  )
