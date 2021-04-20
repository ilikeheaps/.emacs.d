{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:

let
  myEmacs = pkgs.emacs27;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
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
      lua-mode
      magit
      org-bullets
      org-tree-slide
      outshine
      plantuml-mode
      powerline
      rainbow-delimiters
      rust-playground
      sbt-mode
      scala-mode
      sublimity
      tuareg
      use-package
      wgrep-helm
      which-key
      yaml-mode
    ]) ++ (with epkgs.melpaPackages; [
      centered-window
      csharp-mode
      # ^ stable melpa version outputs error: https://github.com/emacs-csharp/csharp-mode/issues/208
      dired-narrow
      proof-general
      # ^ stable melpa version just didn't work
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
