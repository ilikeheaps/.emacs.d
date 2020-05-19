{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs25;
  emacsWithPackages = (pkgs.emacsPackagesGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
      magit
      use-package
      haskell-emacs
      helm
      org-bullets
      org-tree-slide
      outshine
      powerline
      # proof-general # didn't work; used epkgs.proof-general instead
      sr-speedbar
      sublimity
      tuareg
      lsp-ui
      company-lsp
    ]) ++ (with epkgs.melpaPackages; [
    ]) ++ (with epkgs.elpaPackages; [
      debbugs
      ediprolog
      undo-tree
    ]) ++ (with epkgs; [
      adaptive-wrap
      debbugs
      flycheck
      flycheck-status-emoji
      flycheck-ocaml
      centered-window
      plantuml-mode
      which-key
      rainbow-delimiters
      rainbow-mode
      ansible
      lispy
      wgrep-helm
      proof-general
      csharp-mode
      # elm-mode # Emacs Lisp error on startup
      fill-column-indicator
      forth-mode
      fsharp-mode
      flymake
      # haskell-mode # Emacs Lisp error on startup
      # lua-mode # package seems to be broken? hash mismatch
      racket-mode
      rust-playground
      sml-mode
      scala-mode
      sbt-mode
      dired-narrow
      flycheck-haskell
      flycheck-plantuml
      flycheck-rust
      poly-org
      yaml-mode
      csv-mode
    ])
  )
