;;; init.el --- my Emacs config
;;; Commentary:
;; No commentary here, flycheck just wanted me to put these sections here
;;; Code:

;; TODO
;; when migrating to Emacs 26, customize header-line-highlight (see how info mode looks)

;; provides =flet= (and more)
(eval-when-compile (require 'cl))

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

(defun beginning-of-visual-line-1 (&optional n)
  "Perform 'beginning-of-visual-line (+ N 1)' [N=0 for current line]."
  (interactive "P")
  (if (null n) (beginning-of-visual-line)
    (beginning-of-visual-line (+ n 1))
    ))

(defun open-eshell ()
  "Opens a new eshell buffer."
  (interactive)
  (eshell t))

(defun extract-window ()
  "Opens a new frame (with current window) and closes the current window in the old frame."
  (interactive)
  ;; TODO check if there is only one window open
  (make-frame-command)
  (delete-window))

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

(defun try-remove-backup-file-path ()
  "If the current line begins immediately with a path to a backup file (#file#), remove the whole line.  Otherwise go to the next line."
  (interactive)
  (beginning-of-line)
  (if (is-backup-file-path)
      (kill-line 1)
    (forward-line)))

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
    (set-key "<f5>" 'try-remove-backup-file-path)

    (define-key universal-argument-map (kbd "C-l") 'universal-argument-more)
    (define-key universal-argument-map (kbd "C-u") 'scroll-down-line))
  )

;;;; Some settings
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.2)
 '(custom-enabled-themes (quote (misterioso)))
 '(cwm-centered-window-width 100)
 '(delete-active-region nil)
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
    (flycheck-plantuml plantuml-mode outshine haskell-mode proof-general org julia-repl julia-mode debbugs sublimity magit helm sr-speedbar undo-tree org-tree-slide powerline adaptive-wrap centered-window org-bullets fill-column-indicator powerline ediprolog haskell-emacs tuareg markdown-mode sml-mode forth-mode lua-mode elm-mode racket-mode csharp-mode fsharp-mode rust-playground flycheck-status-emoji flycheck-rust flycheck-ocaml flycheck-haskell)))
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
 '(org-level-2 ((t (:inherit outline-3))))
 '(org-level-3 ((t (:inherit outline-2))))
 '(org-level-4 ((t (:inherit outline-5))))
 '(org-level-5 ((t (:inherit outline-4))))
 '(org-meta-line ((t (:inherit font-lock-comment-face :foreground "dark slate gray"))))
 '(powerline-active0 ((t (:inherit mode-line :background "gray44"))))
 '(powerline-active1 ((t (:inherit mode-line :background "gray34" :foreground "white"))))
 '(powerline-active2 ((t (:inherit mode-line :background "gray28" :foreground "white"))))
 '(powerline-inactive0 ((t (:inherit mode-line-inactive :background "gray24"))))
 '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "gray18"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "grey10"))))
 '(show-paren-match ((t (:background "dark slate gray")))))

;;;; settings for MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "This version of Emacs doesn't support SSL connections."))
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; ensure that selected packages are installed
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

;;;; Outline minor mode
(require 'dash)
(require 'outshine)

;;;;; Enable for all programming modes
(add-hook 'prog-mode-hook 'outline-minor-mode)
;;;;; Enable outshine
(add-hook 'outline-minor-mode-hook 'outshine-mode)
;;;;;; Custom regexps for major modes
;; Note that many languages work out of the box (<comment>'*'+ should always work?)
;;;;;; sh-mode
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

;;;; package-dependent global keybinds
(flet ((set-key (str fun) (global-set-key (kbd str) fun)))
  (progn
    (set-key "C-c m" 'magit-status)
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
;;;;; enable flycheck mode
(global-flycheck-mode 1)
;;;;; configure flycheck for Haskell
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
;;;;; flycheck for plantuml
(with-eval-after-load 'flycheck
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))
;;;;; flycheck for Rust
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

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

;;;;; time stamp DONE items (CLOSED: ...)
(setq org-log-done 'time)
;; make a note when closing items
; (setq org-log-done 'note)


;;;;; org-babel-execute for Haskell
(require 'ob-haskell)

;;;;; org-babel-execute for bash
(require 'ob-sh)
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

;;;; (WIP) helm keybindings
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
(require 'company-elisp)
(add-to-list 'company-backends 'company-elisp)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(unless (file-exists-p "~/.emacs.d/config-local.el")
  (copy-file "~/.emacs.d/config-local-template.el"
             "~/.emacs.d/config-local.el"
             nil t nil nil))
(require 'config-local "~/.emacs.d/config-local.el")

;;;; settings for (newly created) frames

(require 'frame)

;; TODO it looks like the very first frame ever doesn't run this
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



;; TODO paren match highlight shadows selection highlight

;;; init.el ends here
