{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs26;
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
      lua-mode
      haskell-mode
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
      elm-mode
      fill-column-indicator
      forth-mode
      fsharp-mode
      flymake
      # lua-mode # package seems to be broken? hash mismatch; used melpaStable above instead
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
      htmlize
    ])
  )
