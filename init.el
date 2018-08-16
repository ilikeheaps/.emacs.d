;;; package --- summary
;;; Commentary:
;; No commentary here, flycheck just wanted me to put these sections here
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (misterioso)))
 '(cwm-centered-window-width 100)
 '(doc-view-resolution 300)
 '(fci-rule-color "dim gray")
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(global-semantic-highlight-func-mode t)
 '(jdee-server-dir "/jar/")
 '(lua-default-application "lua5.3")
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(org-agenda-files (quote ("~/Documents/sushi.org")))
 '(org-babel-load-languages (quote ((emacs-lisp . t))))
 '(org-bullets-bullet-list (quote ("●" "◉" "○")))
 '(org-src-fontify-natively t)
 '(org-structure-template-alist
   (quote
    (("s" "#+BEGIN_SRC ?
#+END_SRC" "<src lang=\"?\">
</src>")
     ("e" "#+BEGIN_EXAMPLE
?
#+END_EXAMPLE" "<example>
?
</example>")
     ("q" "#+BEGIN_QUOTE
?
#+END_QUOTE" "<quote>
?
</quote>")
     ("v" "#+BEGIN_VERSE
?
#+END_VERSE" "<verse>
?
</verse>")
     ("V" "#+BEGIN_VERBATIM
?
#+END_VERBATIM" "<verbatim>
?
</verbatim>")
     ("c" "#+BEGIN_CENTER
?
#+END_CENTER" "<center>
?
</center>")
     ("l" "#+BEGIN_LaTeX
?
#+END_LaTeX" "<literal style=\"latex\">
?
</literal>")
     ("L" "#+LaTeX: " "<literal style=\"latex\">?</literal>")
     ("h" "#+BEGIN_SRC haskell
?
#+END_SRC" "<src lang=\"haskell\">
?
</src>")
     ("H" "#+HTML: " "<literal style=\"html\">?</literal>")
     ("a" "#+BEGIN_ASCII
?
#+END_ASCII" "")
     ("A" "#+ASCII: " "")
     ("i" "#+INDEX: ?" "#+INDEX: ?")
     ("I" "#+INCLUDE: %file ?" "<include file=%file markup=\"?\">")
     ("html " "#+BEGIN_HTML\\n?\\n#+END_HTML" "<literal style=\\\"html\\\">\\n?\\n</literal>"))))
 '(org-todo-keyword-faces (quote (("WAIT" . "dark orange"))))
 '(org-todo-keywords (quote ((sequence "TODO" "WAIT" "DONE"))))
 '(package-selected-packages
   (quote
    (;; dunno
     debbugs
     sublimity
     ;; important utilities
     magit ; cool git
     helm
     ;; not important utilities
     sr-speedbar ; quick access to files but doesn't work well
     undo-tree
     org-tree-slide
     ;; visual stuff
     powerline
     adaptive-wrap
     centered-window
     org-bullets
     fill-column-indicator
     powerline
     ;; programming languages support
     ediprolog
     haskell-emacs
     tuareg ; OCaml stuff
     markdown-mode
     sml-mode
     forth-mode
     lua-mode
     elm-mode
     csharp-mode
     fsharp-mode
     ;; additions for programming languages
     rust-playground
     ;; flycheck stuff
     flycheck-status-emoji
     flycheck-rust
     flycheck-ocaml
     flycheck-haskell)))
 '(semantic-mode t)
 '(tooltip-hide-delay 1200))

;; Turn on horizontal scrolling with mouse wheel
(global-set-key (kbd "<mouse-6>") '(lambda ()
				     (interactive)
				     (scroll-right 1)))
(global-set-key (kbd "<mouse-7>") '(lambda ()
				     (interactive)
				     (scroll-left 1)))

;; various keybinds
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda-list)
(global-set-key (kbd "C-c C-s") 'org-cycle-agenda-files)
;(global-set-key (kbd "<muhenkan> t") 'org-cycle-agenda-files)

;; buffer-menu instead of list-buffers (opens in current window and allows editing)
(global-set-key (kbd "C-x C-b") 'buffer-menu)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-meta-line ((t (:inherit font-lock-comment-face :foreground "dark slate gray")))))

;; settings for MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; (require 'smooth-scroll)
;; (smooth-scroll-mode t)

;; helm stuff
; (require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;;; powerline (this funky status bar)
;; (require 'powerline)
(powerline-default-theme)

;;; enable line wrapping
;; (global-visual-line-mode +1) ; is this +1 different from 1 ?
(global-visual-line-mode)

;;; very cool line wrap indents
(add-hook 'visual-line-mode-hook
 (lambda ()
   (adaptive-wrap-prefix-mode)))
;;; some alternative code:
;;(require 'adaptive-wrap)
;;
;;(with-eval-after-load 'adaptive-wrap
;; (setq-default adaptive-wrap-extra-indent 2))
;;
;;(add-hook 'visual-line-mode-hook
;;  (lambda ()
;;    (adaptive-wrap-prefix-mode +1)
;;    (diminish 'visual-line-mode)))

;; fuck tabs
(setq-default indent-tabs-mode nil)

;;; Haskell mode stuff
;; tags ~~ M-.
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(custom-set-variables '(haskell-tags-on-save t))
;; speedbar support
;; (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
(require 'speedbar)
(speedbar-add-supported-extension ".hs")

;; some magit stuff
(require 'magit-status)
(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-unpushed-to-upstream
                        'magit-insert-unpushed-to-upstream-or-recent
                        'replace)
(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-recent-commits
                        nil t)

;;; Save a list of recent files visited.
;; (recentf-mode 1)

;;; flycheck
;; enable flycheck mode
(global-flycheck-mode 1)
;; configure flycheck for Haskell
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
;; flycheck for Rust
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;;; enable centered-window mode
;; (require 'centered-window-mode)
(when (>= emacs-major-version 25)
  (centered-window-mode t))

  ;; (let
  ;;     ((included '())
  ;;      (excluded '()))
  ;;   (cl-letf ; or cl-flet
  ;;       ((special-buffer-p (buffer)
  ;;                          (let ((buffname (string-trim (buffer-name buffer))))
  ;;                            (and buffname
  ;;                                 (or (string= "*SR-SPEEDBAR*" buffname)
  ;;                                     (and (string-prefix-p "*" buffname)
  ;;                                          (not (string= "*scratch*" buffname))))))))
  ;;     (setq cwm-ignore-buffer-predicates nil))))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
;; Disable menubar
(menu-bar-mode 0)
;; Disable toolbar
(tool-bar-mode 0)


;;;; org-mode

;;; enable pretty bullets
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; prevent org mode from repositioning text when cycling visibility
(remove-hook 'org-cycle-hook
             #'org-optimize-window-after-visibility-change)

;; time stamp DONE items (CLOSED: ...)
(setq org-log-done 'time)
;; make a note when closing items
; (setq org-log-done 'note)


;;; org-babel-execute for Haskell
(require 'ob-haskell)

;;; org-babel-execute for bash
(require 'ob-sh)
;; (org-babel-do-load-languages 'org-babel-load-languages '((sh . t)))

;;Fira Code font
(when (window-system)
  (set-frame-font "Fira Code"))

;;; titlebar format
;; (setq frame-title-format
;;      '(multiple-frames "%b" ("" invocation-name "@" system-name " - %b")))
;; display just the buffer name
(setq frame-title-format "%b")


;;; .emacs ends here
