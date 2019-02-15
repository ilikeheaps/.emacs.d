;;; package --- summary
;;; Commentary:
;; No commentary here, flycheck just wanted me to put these sections here
;;; Code:

;; TODO
;; when migrating to Emacs 26, customize header-line-highlight (see how info mode looks)

;; provides =flet= (and more)
(eval-when-compile (require 'cl))

;;; titlebar format
;; (setq frame-title-format
;;      '(multiple-frames "%b" ("" invocation-name "@" system-name " - %b")))
;; display just the buffer name
(setq frame-title-format "%b")
;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
;; Disable menubar
(menu-bar-mode 0)
;; Disable toolbar
(tool-bar-mode 0)

;; vanilla Emacs global keybinds
(flet ((set-key (str fun) (global-set-key (kbd str) fun)))
  (progn
    ;; Turn on horizontal scrolling with mouse wheel
    (set-key "<mouse-6>" '(lambda ()
                            (interactive)
                            (scroll-right 1)))
    (set-key "<mouse-7>" '(lambda ()
                            (interactive)
                            (scroll-left 1)))
    ;; buffer-menu instead of list-buffers (opens in current window and allows editing)
    (set-key "C-x C-b" 'buffer-menu)
    (set-key "C-u" 'scroll-down-line)
    (set-key "C-e" 'scroll-up-line)
    ;; rebind universal-argument from C-u
    (set-key "C-l" 'universal-argument)
    (define-key universal-argument-map (kbd "C-l") 'universal-argument-more)
    (define-key universal-argument-map (kbd "C-u") 'scroll-down-line))
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.2)
 '(custom-enabled-themes (quote (misterioso)))
 '(cwm-centered-window-width 100)
 '(doc-view-resolution 300)
 '(fci-rule-color "dim gray")
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(global-semantic-highlight-func-mode t)
 '(haskell-tags-on-save t)
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
    (julia-repl julia-mode debbugs sublimity magit helm sr-speedbar undo-tree org-tree-slide powerline adaptive-wrap centered-window org-bullets fill-column-indicator powerline ediprolog haskell-emacs tuareg markdown-mode sml-mode forth-mode lua-mode elm-mode racket-mode csharp-mode fsharp-mode rust-playground flycheck-status-emoji flycheck-rust flycheck-ocaml flycheck-haskell)))
 '(semantic-mode t)
 '(show-paren-style (quote expression))
 '(tooltip-hide-delay 1200))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-debug-newline-face ((t (:background "#f0f0f0" :foreground "dark gray" :weight bold))))
 '(haskell-debug-trace-number-face ((t (:background "#f5f5f5" :foreground "dark gray" :weight bold))))
 '(header-line ((t (:background "gray" :foreground "#333333"))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :foreground "dark slate gray"))))
 '(powerline-active0 ((t (:inherit mode-line :background "gray44"))))
 '(powerline-active1 ((t (:inherit mode-line :background "gray34" :foreground "white"))))
 '(powerline-active2 ((t (:inherit mode-line :background "gray28" :foreground "white"))))
 '(powerline-inactive0 ((t (:inherit mode-line-inactive :background "gray24"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "gray18"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "grey10"))))
 '(show-paren-match ((t (:background "dark slate gray")))))

;; settings for MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; ensure that selected packages are installed
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;; package-dependent global keybinds
(flet ((set-key (str fun) (global-set-key (kbd str) fun)))
  (progn
    (set-key "C-c m" 'magit-status)
    (set-key "C-c l" 'org-store-link)
    (set-key "C-c a" 'org-agenda-list)
    (set-key "C-c C-s" 'org-cycle-agenda-files)
    ))

;(global-set-key (kbd "<muhenkan> t") 'org-cycle-agenda-files)

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
          'adaptive-wrap-prefix-mode)

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
;; tags ~~ M-. : add hasktags to PATH
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

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

;;;; org-mode
(require 'org)
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

;;; OCaml stuff
;; (require 'merlin)
;; ^ TODO where should this go to:
;; a) disable free variable warning
;; b) not load the file when not needed
;; below is a temporary (or not) solution

(defun merlin-my-keybindings ()
  "Keybindings for merlin minor mode."
  ;; TODO do I need xref bindings in Merlin mode?
  (cl-flet* ((def-key (key-str cmd)
               ;; (defvar merlin-mode-map) ; is this okay?
               ;; (require 'merlin) ; this doesn't work
               (define-key merlin-mode-map (kbd key-str) cmd))
             (undef-key (key-str) (def-key key-str nil)))
    (undef-key "C-c C-l") ; merlin-locate
    (undef-key "C-c &")   ; merlin-pop-stack
    (def-key "M-." 'merlin-locate)
    (def-key "M-," 'merlin-pop-stack)
    ;; TODO rebind tuareg-mode: compile from C-c C-c
    (def-key "C-c C-c" 'merlin-error-reset)))
(add-hook 'merlin-mode-hook 'merlin-my-keybindings)

;; TODO how to map 'universal-argument to C-l for helm?
;; this doesn't do it (with helm-major-mode it doesn't work as well)
;;
;; (defun my-helm-keybindings ()
;;   "Keybindings for helm mode."
;;   (cl-flet* ((def-key (key-str cmd)
;;                (define-key helm-mode-map (kbd key-str) cmd))
;;              (undef-key (key-str) (def-key key-str nil)))
;;     (def-key "C-l" 'universal-argument)
;;     ))
;; (add-hook 'helm-mode-hook 'my-helm-keybindings)

(unless (file-exists-p "~/.emacs.d/config-local.el")
  (copy-file "~/.emacs.d/config-local-template.el"
             "~/.emacs.d/config-local.el"
             nil t nil nil))
(require 'config-local "~/.emacs.d/config-local.el")

(require 'frame)
(defun setup-frame-hook (frame)
  "Doc string FRAME."
  (modify-frame-parameters
   frame
   ;; (blink-cursor-alist . '((
   '((cursor-color . "LightSkyBlue3")
     (cursor-type . hbar)
     )
   )
   ;; frame (list (cons 'cursor-color "DeepSkyBlue")))
  ;;Fira Code font when available
  (when (and (window-system) (font-info "Fira Code"))
    (set-frame-font "Fira Code")))

(add-hook 'after-make-frame-functions 'setup-frame-hook)


(if (window-system)
    ;; IF we are not in a TTY, unbind C-m from RET
    (progn
      (define-key input-decode-map [?\C-m] [C-m])
      ;; TODO make it local
      (global-set-key (kbd "<C-m>") 'haskell-process-do-type)))

;; TODO paren match highlight shadows selection highlight

;;; init.el ends here
