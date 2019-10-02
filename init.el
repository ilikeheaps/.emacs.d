;;; init.el --- my Emacs config
;;; Commentary:
;; No commentary here, flycheck just wanted me to put these sections here
;;;; TODO list
;;;;; when migrating to Emacs 26, customize header-line-highlight (see how info mode looks)
;;;;; customize minor modes display
;;;;; lispy mode seems to assume wrong outline format
;;;;; use-package: use delight or diminish
;;;;; use-package: check for more cool stuff (key chords? system packages?)
;;; Code:


;; provides =flet= (and more)
(eval-when-compile (require 'cl))

;; NOTE now this makes customize handicapped because the file isn't loaded -- it can't be loaded because it would conflict with this file's customize sections which I don't want to be automatically changed by customize
(setq custom-file (concat user-emacs-directory "custom.el"))
;;;; garbage collection settings
;; For consideration: set gc-threshold absurdly high just for the init script
;; ~~ after-init-hook
;;;;; Variable and function definitions
(defvar idle-gc-timer nil
  "Timer object for garbage collection on idle.")

(defun disable-idle-gc ()
  "Disable garbage collection on idle."
  (interactive)
  (when idle-gc-timer
    (cancel-timer idle-gc-timer)
    (setq idle-gc-timer nil)))

(defun enable-idle-gc (delay)
  "Enable garbage collection on idle after DELAY."
  (interactive)
  (when idle-gc-timer
    (disable-idle-gc))
  (setq idle-gc-timer
        (run-with-idle-timer
         delay
         t
         (lambda ()
           ;; just for testing purposes
           (garbage-collect)))))

;;;;; Actual settings
(setq garbage-collection-messages nil)
(setq gc-cons-threshold
      (* 120 1024 1024))
(enable-idle-gc 1)
;; (disable-idle-gc)

;;;; titlebar format
;; (setq frame-title-format
;;      '(multiple-frames "%b" ("" invocation-name "@" system-name " - %b")))
;; display just the buffer name
(setq frame-title-format "%b")
;;;;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)
;;;;; Disable menubar
(menu-bar-mode 0)
;;;;; Disable toolbar
(tool-bar-mode 0)

;;;; default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "surf")

;;;; custom defined commands

;;;;; navigation commands
(defun beginning-of-visual-line-1 (&optional n)
  "Perform 'beginning-of-visual-line (+ N 1)' [N=0 for current line]."
  (interactive "P")
  (if (null n) (beginning-of-visual-line)
    (beginning-of-visual-line (+ n 1))
    ))

(defun beginning-of-line-contextual (&optional n)
  "Move cursor to the beginning of text in line; \
if the cursor is already there, move it to the beginning of the line.\
Prefix argument N makes it go N lines down first."
  (interactive "P")
  (let ((n (if n (+ n 1) n))
        (b (point)))
    (beginning-of-line-text n)
    (if (eq (point) b) ; I think this won't ever happen when n is nil
        (beginning-of-line n)))
  )

;;;;; stuff for helper function for managing recover-session files
(defvar regexp-directory-path "/\\([ [:alnum:][:punct:]]*/\\)*")

(defun is-backup-file-path ()
  "Return whether the cursor is immediately before a path to a backup file (#file#)."
  (let ((dir regexp-directory-path))
    (looking-at (concat dir "#[^\n#]*#"))))

;; just an alias, maybe name different?
(defun end-of-line-exc (&optional n)
  "An alias to `(point-at-eol N)`."
    (point-at-eol n))

(defun end-of-line-p (&optional pos)
  "Check whether position POS is at the end of current line.  Check current point position if no position is given."
  (let ((pos (or pos (point))))
    (eq pos (end-of-line-exc))))

(defun linkify-path-or-kill-line ()
  "If the current line begins (ignoring leading whitespace) with a path to a backup file (#file#), remove the whole line (includes whitespace-only lines).  Otherwise indent (org-cycle) and wrap with brackets to make into org link and move cursor to the next line."
  (interactive)
  (beginning-of-line)
  (skip-chars-forward " ")
  (if (or (is-backup-file-path) (end-of-line-p))
      (progn (beginning-of-line)
             (kill-line 1))
    (progn
      (org-indent-line)
      (insert "[[")
      (end-of-line)
      (insert "]]")
      (forward-line)
      (beginning-of-line))))

;;;;; other utility commands
(defun open-eshell ()
  "Opens a new eshell buffer."
  (interactive)
  (eshell t))

(defun open-info (&optional file-or-node-opt)
  "Open a new info buffer.  If FILE-OR-NODE-OPT isn't provided, ask interactively for one."
  (interactive)
  (let ((file-or-node (or file-or-node-opt (read-string "info node: "))))
    (info file-or-node (generate-new-buffer (concat "*<info:" file-or-node ">*")))))

(defun extract-window ()
  "Opens a new frame (with current window) and closes the current window in the old frame."
  (interactive)
  ;; TODO check if there is only one window open
  (make-frame-command)
  (delete-window))

(defun clone-indirect-buffer-select (&optional buffer newname norecord)
  "Make an indirect copy of the specified BUFFER and return it.  If BUFFER is nil, use the current buffer.  NEWNAME and NORECORD arguments are passed to `clone-indirect-buffer` under the hood."
  (let ((buf (or buffer (current-buffer))))
    (with-current-buffer buf
      (clone-indirect-buffer newname nil norecord))))

(defun clone-indirect-buffer-new-frame (&optional buffer)
  "Make a new frame with an indirect copy of the specified BUFFER.  If called interactively or with nil BUFFER argument, use the current buffer.  Return the new window in the new frame (probably)."
  (interactive)
  (display-buffer (clone-indirect-buffer-select buffer) '(display-buffer-pop-up-frame . ())))

;;;; vanilla Emacs global keybinds

(put 'narrow-to-region 'disabled nil)

(define-prefix-command 'ctrl-o-prefix)
(define-prefix-command 'meta-s-prefix)

(flet ((set-key (str fun) (global-set-key (kbd str) fun))
       (three-level (str fun1 fun2 fun3)
                    (set-key (concat "C-" str) fun1)
                    (set-key (concat "C-S-" str) fun2)
                    (set-key (concat "M-" str) fun3)))
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
    (set-key "C-d" 'open-eshell)
    ;; directions
    (three-level "f"
                 'previous-line
                 'beginning-of-buffer
                 'backward-sentence)
    ;; (set-key "C-f" 'previous-line)
    ;; (set-key "C-S-f" 'beginning-of-buffer)
    (three-level "s"
                 'next-line
                 'end-of-buffer
                 'forward-sentence)
    ;; (set-key "C-s" 'next-line)
    ;; (set-key "C-S-s" 'end-of-buffer)
    (three-level "r"
                 'backward-char
                 'beginning-of-line-contextual
                 'backward-word)
    ;; (set-key "C-r" 'backward-char)
    ;; (set-key "C-S-r" 'beginning-of-line-contextual)
    (three-level "t"
                 'forward-char
                 'end-of-line
                 'forward-word)
    ;;(set-key "C-t" 'forward-char)
    ;;(set-key "C-S-t" 'end-of-line)
    (set-key "C-a" 'beginning-of-visual-line-1) ; TODO consider C-???-r instead
    (set-key "C-p" 'isearch-forward)
    (set-key "C-w" 'isearch-backward)
    (set-key "C-n" 'kill-region) ; TODO find a better key, n/i look good for indents
    (set-key "C-o" 'ctrl-o-prefix)
    (set-key "C-o C-n" 'make-frame-command)
    (set-key "C-o C-e" 'extract-window)
    (set-key "C-o <C-i>" 'clone-indirect-buffer-new-frame)
    (set-key "<f5>" 'linkify-path-or-kill-line)

    (define-key universal-argument-map (kbd "C-l") 'universal-argument-more)
    (define-key universal-argument-map (kbd "C-u") 'scroll-down-line))
  )

;;;; Some settings
;; moving away from Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(centered-window-mode nil)
 '(company-idle-delay 0.2)
 '(custom-enabled-themes (quote (misterioso)))
 '(cwm-centered-window-width 100)
 '(delete-active-region nil)
 '(doc-view-resolution 300)
 '(fci-rule-color "dim gray")
 '(global-semantic-highlight-func-mode t)
 '(haskell-tags-on-save t)
 '(jdee-server-dir "/jar/")
 '(lua-default-application "lua5.3")
 '(magit-display-buffer-function (quote magit-display-buffer-same-window-except-diff-v1))
 '(mouse-1-click-follows-link 200)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 1) ((control)))))
 '(org-activate-links (quote (bracket angle radio tag date footnote)))
 '(org-agenda-files (quote ("~/Documents/sushi.org")))
 '(org-agenda-tags-column -100)
 '(org-babel-load-languages (quote ((emacs-lisp . t))))
 '(org-bullets-bullet-list (quote ("●" "◉" "○")))
 '(org-id-link-to-org-use-id t)
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
 '(org-tags-column -100)
 '(semantic-mode t)
 '(show-paren-style (quote expression))
 '(tooltip-hide-delay 1200))

(setq org-todo-keyword-faces '(("WAIT" . "dark orange")
                               ("FAIL" . "grey")))
(setq org-todo-keywords '((sequence "TODO(a)" "WAIT(r)" "|" "DONE(s)" "FAIL(t)")))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-even-diff-A ((t (:background "#343551"))))
 '(ediff-odd-diff-A ((t (:background "#343551"))))
 '(ediff-even-diff-B ((t (:background "#343551"))))
 '(ediff-odd-diff-B ((t (:background "#343551"))))
 '(ediff-even-diff-C ((t (:background "#343551"))))
 '(ediff-odd-diff-C ((t (:background "#343551"))))
 '(lsp-ui-sideline-global ((t (:background "#222235"))))
 '(term-color-blue ((t (:foreground "#AA88FF"))))
 ;; there are also a few other defined terminal colours (8 in total?)
 '(fringe ((t (:background "#1d2733"))))
 '(haskell-debug-newline-face ((t (:background "#f0f0f0" :foreground "dark gray" :weight bold))))
 '(haskell-debug-trace-number-face ((t (:background "#f5f5f5" :foreground "dark gray" :weight bold))))
 '(header-line ((t (:background "gray" :foreground "#333333"))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :foreground "dark slate gray"))))
 '(powerline-inactive0 ((t (:inherit mode-line :background "gray44"))))
 '(powerline-inactive1 ((t (:inherit mode-line :background "gray34" :foreground "white"))))
 '(powerline-inactive2 ((t (:inherit mode-line :background "gray28" :foreground "white"))))
 '(powerline-active0 ((t (:inherit mode-line-inactive :background "gray24"))))
 '(powerline-active1 ((t (:inherit mode-line-inactive :background "gray18"))))
 '(powerline-active2 ((t (:inherit mode-line-inactive :background "grey10"))))
 '(show-paren-match ((t (:background "dark slate gray")))))


(require 'outline)
;; TODO set-face-attribute is supposed to be used internally, what's a good function to use instead?
(set-face-attribute 'outline-1 nil :foreground "cyan")
(set-face-attribute 'outline-2 nil :foreground "deep sky blue")
(set-face-attribute 'outline-3 nil :foreground "chartreuse3")
(set-face-attribute 'outline-4 nil :foreground "gold2")
(set-face-attribute 'outline-5 nil :foreground "sandy brown")
(set-face-attribute 'outline-6 nil :foreground "yellow green")
(set-face-attribute 'outline-7 nil :foreground "goldenrod")
(set-face-attribute 'outline-7 nil :foreground "dark orange")
;; other candidates:
;; "green yellow" -- very strong
;; "lime green"
;; "light sky blue"
;; "medium purple"
;; "light slate blue"
;; "DarkOrange2"
;; "RoyalBlue1"
;; "MediumOrchid1"
;; "VioletRed1"

;;;; settings for MELPA and packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "This version of Emacs doesn't support SSL connections."))
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))

;;;;; ensure that use-package is installed
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents)
  (package-install 'use-package))
;; see https://github.com/jwiegley/use-package
(require 'use-package)
;;;;; setup use-package
;; (require 'use-package-always-ensure) ; TODO is this needed?
(setq use-package-always-ensure t)

;;;; don't use default auto-mode-alist
;; So that only modes defined from now on will be bound to extensions.
(when (not (boundp 'vanilla-auto-mode-alist))
  (defvar vanilla-auto-mode-alist auto-mode-alist
    "`auto-mode-alist` as it is in default Emacs.")
  (setq auto-mode-alist nil))

;;;;; Rebind modes present in vanilla Emacs
;;;;;; Emacs lisp
(use-package emacs-lisp-mode
  :ensure nil
  :mode "\\.el\\'")
;;;; Uncustomized packages (converting from Customize's `selected-packages`)
;;;;;; adaptive-wrap
(use-package adaptive-wrap
  :commands adaptive-wrap-prefix-mode)
;;;;;; centered-window
(use-package centered-window
  :commands centered-window-mode)
;;;;;; csharp-mode
(use-package csharp-mode
  :mode "\\.cs$")
;;;;;; debbugs
(use-package debbugs
  :demand)
;;;;;; ediprolog
(use-package ediprolog
  :demand)
;;;;;; elm-mode
(use-package elm-mode
  :mode "\\.elm\\'"
  :ensure t)
;;;;;; fill-column-indicator
(use-package fill-column-indicator
  :commands fci-mode)
;;;;;; forth-mode
(use-package forth-mode
  :mode "\\.\\(f\\|fs\\|fth\\|4th\\)\\'")
;;;;;; fsharp-mode
(use-package fsharp-mode
  :mode "\\.fs[iylx]?\\'")
;;;;;; haskell-emacs
(use-package haskell-emacs
  :demand)
;;;;;; haskell-mode
(use-package
  haskell-mode
  :mode (("\\.hsc\\'" . haskell-mode)
         ("\\.l[gh]s\\'" . literate-haskell-mode)
         ("\\.hsig\\'" . haskell-mode)
         ("\\.[gh]s\\'" . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)))
;;;;;; helm
(use-package helm
  :demand)
;;;;;; lua-mode
(use-package lua-mode
  :mode "\\.lua\\'")
;;;;;; magit
(use-package magit
  :commands (magit magit-clone magit-init))
;;;;;; markdown-mode
;; TODO? ("\\.markdown\\'" . markdown-mode)
(use-package markdown-mode
  :mode "\\.md\\'")
;;;;;; org-mode
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :ensure t)
;;;;;; org-bullets
(use-package org-bullets
  :demand)
;;;;;; TODO org-tree-slide
;; does this even work?
(use-package org-tree-slide
  :demand)
;;;;;; outshine
(use-package outshine
  :demand)
;;;;;; plantuml-mode
(use-package plantuml-mode
  :mode "\\.\\(plantuml\\|pum\\|plu\\)\\'")
;;;;;; powerline
(use-package powerline
  :demand)
;;;;;; proof-general
;; TODO auto mode alist entry?
(use-package proof-general
  :demand)
;;;;;; racket-mode
(use-package racket-mode
  :mode "\\.rkt[dl]?\\'")
;;;;;; rust-playground
(use-package rust-playground
  :commands (rust-playground rust-playground-mode))
;;;;;; sml-mode
(use-package sml-mode
  :mode (("\\.grm\\'" . sml-yacc-mode)
         ("\\.cm\\'" . sml-cm-mode)
         ("\\.s\\(ml\\|ig\\)\\'" . sml-mode)))
;;;;;; sr-speedbar
(use-package sr-speedbar
  :demand)
;;;;;; sublimity
(use-package sublimity
  :demand)
;;;;;; tuareg
(use-package tuareg
  :demand)
;;;;;; undo-tree
(use-package undo-tree
  :demand)

;;;; Scala development (scala-mode, sbt-mode, lsp-mode, metals)
(use-package scala-mode
  :mode "\\.\\(scala\\|sbt\\)\\'")

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package lsp-mode
  :ensure t
  :hook (scala-mode . lsp)
  :bind (:map lsp-mode-map
              ("M-." . my-lsp-find-definition)
              ("M-," . my-lsp-history-pop))
  :config
  (defvar my-lsp-history nil
    "History of lsp navigation jumps.")

  (defun my-lsp-find-definition ()
    "`lsp-find-definition` but keeping position in a dedicated stack"
    (interactive)
    (push (cons (point) (current-buffer)) my-lsp-history)
    (lsp-find-definition))

  (defun my-lsp-history-pop ()
    "Return to the position before the previous jump."
    (interactive)
    (if my-lsp-history
        (let ((pos (pop my-lsp-history)))
          (switch-to-buffer (cdr pos))
          (goto-char (car pos)))
      (message "There is no more history")))

  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-snippet nil))

(use-package lsp-ui
  :ensure t)

(use-package company-lsp
  :ensure t)

;;;; Outline minor mode
(require 'dash)
(require 'outshine)

;;;;; Enable for all programming modes
(add-hook 'prog-mode-hook 'outline-minor-mode)
;;;;; Enable outshine
(add-hook 'outline-minor-mode-hook 'outshine-mode)
;;;;;; Custom regexps for major modes
;; Note that many languages work out of the box (<comment>'*'+ should always work?)
;;;;;;; TODO sh-mode
;; see outline for haskell-mode
;; TODO doesn't work
;; (defun outline-calculate-sh-mode-level ()
;;   "Calculate the level of a ##+ headline.  Assume the point is before a proper headline."
;;   (save-excursion
;;     (save-match-data
;;       (forward-char)
;;       (let ((count 0))
;;         (while (looking-at "#")
;;           (setq count (+ count 1))
;;           (forward-char))
;;         count))))
;; (defun outline-set-sh-mode-headlines ()
;;   "Set outline regex in `sh-mode` to accept ##+ (up to 9 characters giving 8 levels)."
;;   (setq-local outline-regexp "#[#]\\{1,8\\}")
;;   (setq-local outline-level 'outline-calculate-sh-mode-level))
;; (remove-hook 'sh-mode-hook 'outline-set-sh-mode-headlines)
;; (add-hook 'sh-mode-hook 'outline-set-sh-mode-headlines)
;;;;;;; TODO haskell-mode
(defun generic-outline-level (regex-skip regex-count &optional offset)
  (save-excursion
    (save-match-data
      (looking-at regex-skip)
      (goto-char (match-end 0))
      (looking-at regex-count)
      (- (match-end 0) (match-beginning 0) (or offset 0)))))

;; TODO lambda sees dynamic bindings?
(defun setup-outline-generic (regex-skip regex-count &optional offset)
  (setq-local outline-regexp (concat regex-skip regex-count))
  (setq-local outline-level (lambda () (generic-outline-level regex-skip regex-count offset))))

(defun setup-outline-for-haskell ()
  (setup-outline-generic "--[ ]*" "[*]\\{1,8\\}"))

;; (add-hook 'haskell-mode-hook 'outline-for-haskell-mode)
;;;; package-dependent global keybinds
;; TODO configuration for specific packages could be done with (use-package ... :config ...)
;;      but it would also make it easier to make overlapping keybinds :c
(flet ((set-key (str fun) (global-set-key (kbd str) fun)))
  (progn
    (set-key "<C-m>" 'magit-status) ;; NOTE only works in GUI!
    ;; TODO don't use C-c prefix
    (set-key "C-c l" 'org-store-link)
    (set-key "C-c a" 'org-agenda-list)
    (set-key "C-c C-s" 'org-cycle-agenda-files)

    ;; helm stuff
    ;; (require 'helm-config)
    (set-key "M-x" 'helm-M-x)
    (set-key "C-x C-f" 'helm-find-files)
    ))

;(global-set-key (kbd "<muhenkan> t") 'org-cycle-agenda-files)

;;;; smooth-scroll (disabled, didn't really like)
;; (require 'smooth-scroll)
;; (smooth-scroll-mode t)

;;;; powerline (this funky status bar)
;; (require 'powerline)
(powerline-default-theme)

;;;; enable line wrapping
;; (global-visual-line-mode +1) ; is this +1 different from 1 ?
(global-visual-line-mode)

;;;; search only visible text
;; Use M-s i during isearch to change behaviour on the fly
(setq search-invisible nil)
;;;;; for org-mode only (not using)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (make-local-variable 'search-invisible)
;;             (setq search-invisible nil)))
;;;; very cool line wrap indents
(add-hook 'visual-line-mode-hook
          'adaptive-wrap-prefix-mode)

;;;;; some alternative code:
;;(require 'adaptive-wrap)
;;
;;(with-eval-after-load 'adaptive-wrap
;; (setq-default adaptive-wrap-extra-indent 2))
;;
;;(add-hook 'visual-line-mode-hook
;;  (lambda ()
;;    (adaptive-wrap-prefix-mode +1)
;;    (diminish 'visual-line-mode)))

;;;; fuck tabs
(setq-default indent-tabs-mode nil)

;;;; Haskell mode stuff
;; tags ~~ M-. : add hasktags to PATH
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

;;;; speedbar support
;; (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
(require 'speedbar)
(speedbar-add-supported-extension ".hs")

;;;; some magit stuff
;; TODO this is slow -- defer?
(require 'magit-status)
(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-unpushed-to-upstream
                        'magit-insert-unpushed-to-upstream-or-recent
                        'replace)
(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-recent-commits
                        nil t)

;;;; Save a list of recent files visited.
;; (recentf-mode 1)

;;;; flycheck
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled idle-buffer-switch)
        flycheck-display-errors-function nil))

;;;;; use emoji for status icons
(use-package flycheck-status-emoji
  :demand)

;;;;; custom flycheck prefix
(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-a C-f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map))
;;;;; configure flycheck for Haskell
(use-package flycheck-haskell
  :hook (flycheck-mode-hook . flycheck-haskell-setup))
;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;;;;; flycheck for plantuml
(use-package flycheck-plantuml
  ;; TODO should this be in a hook or just once after loading flycheck?
  :hook (flycheck-mode-hook . flycheck-plantuml-setup))

;;;;; flycheck for Rust
(use-package flycheck-rust
  :hook (flycheck-mode-hook . flycheck-rust-setup))

;;;;; flycheck for OCaml
(use-package flycheck-ocaml
  :demand)

;;;; enable centered-window mode
;; (require 'centered-window-mode)
(when (>= emacs-major-version 25)
  (centered-window-mode t))

;;;; org-mode
(require 'org)
;;;;; enable pretty bullets
(add-hook 'org-mode-hook 'org-bullets-mode)

;;;;; prevent org mode from repositioning text when cycling visibility
(remove-hook 'org-cycle-hook
             #'org-optimize-window-after-visibility-change)

;;;;; org-src settings
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-strip-leading-and-trailing-blank-lines t)

;; didn't help :(
;; ;; remove comments from org document for use with export hook
;; ;; https://emacs.stackexchange.com/questions/22574/orgmode-export-how-to-prevent-a-new-line-for-comment-lines
;; (defun delete-org-comments (backend)
;;   (loop for comment in (reverse (org-element-map (org-element-parse-buffer)
;;                     'comment 'identity))
;;     do
;;     (setf (buffer-substring (org-element-property :begin comment)
;;                 (org-element-property :end comment))
;;           "")))
;;
;; ;; add to export hook
;; (remove-hook 'org-export-before-processing-hook 'delete-org-comments)

;;;;; time stamp DONE items (CLOSED: ...)
(setq org-log-done 'time)
;; make a note when closing items
; (setq org-log-done 'note)


;;;;; org-babel-execute for Haskell
(require 'ob-haskell)

;;;;; TODO org-babel-execute for bash
;; this didn't work on a new machine
;; (require 'ob-sh)
;; (org-babel-do-load-languages 'org-babel-load-languages '((sh . t)))

;;;; OCaml stuff
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

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
(add-hook 'merlin-mode-hook 'merlin-my-keybindings t)

;;;; helm keybindings
(with-eval-after-load 'helm
  (cl-flet* ((def-key (key-str cmd)
               (define-key helm-map (kbd key-str) cmd))
             (undef-key (key-str) (def-key key-str nil)))
    (def-key "C-l" 'universal-argument)
    (def-key "C-f" 'helm-previous-line)
    (def-key "C-s" 'helm-next-line)
    ))
;; TODO is this part needed?
(with-eval-after-load 'helm-types
  (cl-flet* ((def-key (key-str cmd)
               (define-key helm-generic-files-map (kbd key-str) cmd))
             (undef-key (key-str) (def-key key-str nil)))
    (def-key "C-p" 'helm-ff-run-grep)
    (def-key "C-s" 'helm-next-line)
    ))
(with-eval-after-load 'helm-files
  (cl-flet* ((def-key (key-str cmd)
               (define-key helm-find-files-map (kbd key-str) cmd))
             (undef-key (key-str) (def-key key-str nil)))
    (def-key "C-p" 'helm-ff-run-grep)
    (def-key "C-s" 'helm-next-line)
    (def-key "C-l" 'universal-argument)
    (def-key "C-n" 'helm-find-files-up-one-level)
    ))

;;;;; Snippet for checking if bindings are correct
;; (lookup-key helm-map (kbd "C-s"))
;; (lookup-key helm-find-files-map (kbd "C-l"))
;; (lookup-key helm-generic-files-map (kbd "C-s"))

;;;; org-mode agenda options
(progn
  ;; TODO consider whether this loads code into memory and stuff
  (require 'org-agenda)
  ;;open agenda in current window
  (setq org-agenda-window-setup `current-window)
  ;;warn me of any deadlines in next 14 days
  (setq org-deadline-warning-days 14)
  ;;show me tasks scheduled or due in...
  (setq org-agenda-span 8) ;; TODO when =7 it show the current week instead of next n days
  ;;
  (setq org-agenda-skip-scheduled-if-deadline-is-shown nil)
  ;;don't give awarning colour to tasks with impending deadlines if they are scheduled to be done
  ;; (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
  ;;don't show tasks that are scheduled or have deadlines in the normal todo list
  ;; (setq org-agenda-todo-ignore-deadlines (quote all))
  ;; (setq org-agenda-todo-ignore-scheduled (quote all))
  (setq org-agenda-sorting-strategy
        '((agenda time-up timestamp-up deadline-up priority-down)
          (todo priority-down category-keep)
          (tags priority-down category-keep)
          (search category-keep)))
  )

;;;; company elisp
(use-package company
  :commands (company-mode)
  :config
  (add-to-list 'company-backends 'company-elisp)
  :init
  (add-hook 'emacs-lisp-mode-hook 'company-mode))

;;;; local config
(unless (file-exists-p "~/.emacs.d/config-local.el")
  (copy-file "~/.emacs.d/config-local-template.el"
             "~/.emacs.d/config-local.el"
             nil t nil nil))
(require 'config-local "~/.emacs.d/config-local.el")

;;;; settings for (newly created) frames

(require 'frame)

;; TODO it looks like the very first frame ever doesn't run this
;; NOTE see initial-frame-alist
(defun setup-frame (frame)
  "Function to perform on every newly created FRAME."
  ;; just for testing (see TODO above)
  (print "setup-frame-hook peformed!")
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
    ;; distinguish between <C-m> and RET (GUI only)
    (define-key input-decode-map [?\C-m] [C-m])
    ;; distinguish between <C-i> and TAB (GUI only)
    (define-key input-decode-map [?\C-i] [C-i])
    (set-frame-font "Fira Code" nil (list frame))
    ))

(when (window-system)
  (setup-frame (selected-frame)))

(add-hook 'after-make-frame-functions 'setup-frame)

;;;; plantuml settings (binary etc.)

(require 'plantuml-mode)
(let ((path (expand-file-name "~/bin/plantuml.jar")))
  (setq plantuml-jar-path path)
  (setq org-plantuml-jar-path path))
(setq plantuml-output-type "png")

;;;; org-babel languages

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (emacs-lisp . t)))


;; TODO paren match highlight shadows selection highlight

;;;; show completions of the current key stroke (which-key)
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-right-bottom)
  :bind ("C-h B" . which-key-show-top-level))
;;;; colour delimiters (e.g. parens) by depth (rainbow-delimiters)
(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
;;;; colour colour codes (rainbow-mode)
(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode))

;;;; enable dired-find-alternate-file
(put 'dired-find-alternate-file 'disabled nil)

;;;; Use multiple major modes in org buffers (poly-org)
;; this will (almost) seamlessly switch between major modes in org buffers depending on the context (code blocks, ...?)
(use-package poly-org
  :commands poly-org-mode
  ;; it becomes a bit laggy in somewhat big files (like my personal .org)
  ;; :mode ("\\.org\\'" . poly-org-mode)
  :ensure t)

;;;; editing ansible files
;;;;; yaml major mode (yaml-mode)
(use-package yaml-mode
  :mode "\\.\\(e?ya?\\|ra\\)ml\\'"
  :ensure t)
;;;;; ansible minor mode (ansible)
(use-package ansible
  :ensure t
  :init
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1))))

;;;; editing lisp (lispy)
(use-package lispy
  :ensure t
  :init
  (add-hook 'lisp-mode-hook 'lispy-mode)
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode))

;;;; edit helm grep results (wgrep-helm)
(use-package wgrep-helm
  :ensure t)

;;;; edit csv files (csv-mode)
(use-package csv-mode
  :ensure t
  :mode "\\.csv\\'"
  :config (add-hook 'csv-mode-hook (lambda () (toggle-truncate-lines 1))))

;;; init.el ends here
