;; -*- coding: utf-8; lexical-binding: t -*-
;; .emacs - finally got this just how i like it

;; Package configuration ======================================================

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)  ;; auto-install packages, YOLO

(use-package flycheck)
(use-package magit)
(use-package multiple-cursors)
(use-package yasnippet)

(use-package haskell-mode)
(use-package lua-mode)
(use-package markdown-mode)
(use-package protobuf-mode)
(use-package terraform-mode)
(use-package yaml-mode)
(use-package wgsl-mode)
(use-package boogie-friends)

(use-package lean-mode)
(use-package company-lean)

;; I don't know what this is, but it looks so terrifyingly important I will never delete it.
(use-package xref
  :pin gnu)

(use-package eldoc
  :pin gnu
  :bind ("s-d" . #'eldoc)
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil))

(use-package xah-math-input
  :bind (:map xah-math-input-keymap
              ("C-x C-SPC" . xah-math-input-change-to-symbol)
              ("M-S-SPC" . xah-math-input-change-to-symbol))

  :config
  (puthash "<" "⟨" xah-math-input-abrvs)
  (puthash ">" "⟩" xah-math-input-abrvs)
  (puthash "ring" "∘" xah-math-input-abrvs)
  (puthash "compose" "∘" xah-math-input-abrvs)
  (puthash "check" "✓" xah-math-input-abrvs)
  (puthash "<|" "⊲" xah-math-input-abrvs)
  (puthash "|>" "⊳" xah-math-input-abrvs)
  (puthash "_|_" "⊥" xah-math-input-abrvs)
  (puthash "<>" "◇" xah-math-input-abrvs)
  (puthash "[]" "◻" xah-math-input-abrvs)
  (puthash "~" "¬" xah-math-input-abrvs)
  (puthash "|=" "⊨" xah-math-input-abrvs)

  ;; pedantically insist on U+22C5 DOT OPERATOR rather than U+2022 BULLET
  ;; or, like, U+2219 BULLET OPERATOR, etc. ad nauseam.
  (xah-math-input--add-cycle ["*" "⋅" "•" "×"]) ; multiply, times

  ;; I don't like the default key-binding.
  (define-key xah-math-input-keymap (kbd "S-SPC") nil)
  (global-xah-math-input-mode))

(use-package rg
  :init
  (rg-define-search jorendorff-rg-literal-in-project
    :format literal
    :files current
    :flags ("--sort" "path")
    :dir project)
  :bind ("M-g r" . #'jorendorff-rg-literal-in-project))

(use-package lsp-mode
  :ensure
  :commands lsp
  :hook rust-mode
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all nil)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints nil)
  ; waaay too obstructive
  ;:config
  ;(add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

'(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package rust-mode
  :mode "\\.rs\\'"
  :bind (:map rust-mode-map
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c t" . lsp-find-type-definition)))

;; Failed attempt to use eglot and rustic =====================================
;;
;; (use-package eglot
;;   :hook ((rustic-mode) . eglot-ensure))
;;
;; (use-package rust-mode
;;   :hook (rust-mode . flycheck-mode))
;;
;; (use-package rustic
;;   :init
;;   (setq rustic-lsp-client 'eglot)
;;   ;; Turn off flymake.
;;   (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
;;   ;; eglot-format on save.
;;   (add-hook 'rustic-mode-hook
;;             (lambda () (add-hook 'before-save-hook
;;                                  (lambda () (when (eq major-mode 'rustic-mode)
;;                                               (eglot-format-buffer)))
;;                                  nil
;;                                  'local))))

;; Other junk =================================================================

(add-to-list 'load-path "~/src/boogie-friends/emacs/")

;; Enable company globally for all modes
;;(global-company-mode)
;;
;; Reduce the time after which the company autocomplete popup opens
;;(setq company-idle-delay 0.2)
;;
;; Reduce the number of characters before company kicks in
;;(setq company-minimum-prefix-length 1)

;; hippie-expand! should try using M-/ for this
;;(define-key global-map [(meta ?\\)] 'hippie-expand)

(define-key ctl-x-map [?!] 'window-swap-states)


(defun work-log ()
  "Open my work log."
  (interactive)
  (find-file (concat (getenv "HOME") "/misc/work-log.md")))

(define-key global-map (kbd "C-x w") 'work-log)


;; Custom hack for pasting bits of GitHub UI text into my work journal.
(defun jorendorff--insert-for-yank (orig-insert-for-yank string)
  (apply orig-insert-for-yank
   (if (and (string= (buffer-file-name) "/Users/jorendorff/misc/work-log.md")
            (string-match "^ \\(.*?\\)\\.? \\(#[0-9]+\\) $" string))
       (replace-match "\"\\1\", \\2" t nil string)
     string)
   nil))

(advice-add 'insert-for-yank :around #'jorendorff--insert-for-yank)


(defun gcr/plist-to-alist (ls)
  "Convert a plist to an alist. Primarily for old color-theme themes."
  (let ((result nil))
    (while ls
      (add-to-list 'result (cons (car ls) (cadr ls)))
      (setq ls (cddr ls)))
    result))
(defalias 'plist-to-alist 'gcr/plist-to-alist)

(require 'page-ext)

(add-hook 'dafny-mode-hook #'(lambda ()
                               (company-mode 0)
                               (auto-composition-mode 0)
                               (set (make-local-variable 'tab-width) 4)))



(push "/opt/local/share/emacs/site-lisp" load-path)
(autoload 'gid "idutils" nil t)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))


;; Turning stuff off that has no business existing ============================

;; Don't vanish and lose keyboard focus when I type Ctrl-Z, the standard key
;; for undo on Linux and Windows.
(define-key global-map [(control ?z)] nil)
(define-key ctl-x-map [(control ?z)] nil)

;; Disable ns-popup-font-panel on Mac
(global-unset-key (kbd "s-t"))

;; No nasty tool-bar! good grief
(tool-bar-mode 0)

;; mouse-6 is triggered by two-finger-scrolling to the right; mouse-7 to the left
(global-set-key [mouse-6] (function (lambda () (interactive) nil)))
(global-set-key [mouse-7] (function (lambda () (interactive) nil)))


;; My custom tweaks ===========================================================

;; Show column numbers in mode line.
(column-number-mode t)

;; When cursor is on a bracket, highlight the matching bracket.
(show-paren-mode t)

;; Emoji on Mac 🚀🍩💖
(when (member "Apple Color Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

;; Save before grepping, thanks to Jim Blandy.
(defadvice grep (before save-before-grepping)
  (save-some-buffers))
(ad-activate 'grep)
(defadvice grep-find (before save-before-grepping)
  (save-some-buffers))
(ad-activate 'grep-find)

;; Cycle through grep hits with C-`.
(global-set-key [?\C-`] 'next-error)
(global-set-key [?\C-~] 'previous-error)

(global-set-key "\M-q"  'fill-paragraph)

;; C-< and C->
(defun indent-rigidly-4 (start end)
  (interactive "r")
  (indent-rigidly start end 4))
(global-set-key [?\C->] 'indent-rigidly-4)
(defun dedent-rigidly-4 (start end)
  (interactive "r")
  (indent-rigidly start end -4))
(global-set-key [?\C-<] 'dedent-rigidly-4)

(defun jimb-diff-mode-hook ()
  (define-key diff-mode-map "\M-q" nil))
(add-hook 'diff-mode-hook 'jimb-diff-mode-hook)

;; Change silly defaults for Home and End keys on Mac
(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)

;; Things to do only when running in GUI mode:
(when (not (null (window-system)))
  (server-start)
  (setq confirm-kill-emacs 'yes-or-no-p))


;; Mac-like key bindings ======================================================

;; Cmd+A to select all
(defun select-all ()
  (interactive "")
  (set-mark (point-min))
  (goto-char (point-max)))
(global-set-key [(super ?a)] 'select-all)

;; Cmd+G to find again.
;; Bind s-g and s-G to "search again", like C-s C-s and C-r C-r.
(defun jorendorff-isearch-again-forward ()
  "Do the equivalent of C-s C-s."
  (interactive)
  ;; One of these "nil"s means: no recursive-edit here, i.e. isearch-mode
  ;; should return immediately rather than whenever the user is done searching.
  (isearch-mode t nil nil nil)
  (isearch-repeat-forward))
(global-set-key (kbd "s-g") 'jorendorff-isearch-again-forward)
(define-key isearch-mode-map [(super ?g)] 'isearch-repeat-forward)
(defun jorendorff-isearch-again-backward ()
  "Do the equivalent of C-r C-r."
  (interactive)
  (isearch-mode nil nil nil nil)
  (isearch-repeat-backward))
(global-set-key (kbd "s-G") 'jorendorff-isearch-again-backward)
(define-key isearch-mode-map [(super ?G)] 'isearch-repeat-backward)

;; Cmd+W to close a buffer.
(defun kill-current-buffer ()
  "Kill the current buffer without prompting which buffer to kill."
  (interactive)
  (kill-buffer))
(global-set-key (kbd "s-w") 'kill-current-buffer)

;; C-tab to switch buffers.
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)  ;; linux :-|
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)  ;; mac :-\


;; HTML entities (key bindings chosen to match MacOS defaults) ================

(defun insert-ldquo () (interactive) (insert "“"))
(define-key global-map "\M-[" 'insert-ldquo)

(defun insert-rdquo () (interactive) (insert "”"))
(define-key global-map "\M-{" 'insert-rdquo)

(defun markdown-mode-keymap-smackdown ()
  (local-unset-key "\M-}")
  (local-unset-key "\M-{"))
(add-hook 'markdown-mode-hook 'markdown-mode-keymap-smackdown)

(defun insert-lsquo () (interactive) (insert "‘"))
(define-key global-map "\M-]" 'insert-lsquo)

(defun insert-rsquo () (interactive) (insert "’"))
(define-key global-map "\M-}" 'insert-rsquo)

(defun insert-mdash () (interactive) (insert "—"))
(define-key global-map "\M-_" 'insert-mdash)


;; For package magit ==========================================================
(global-set-key (kbd "C-x g") 'magit-status)


;; ;; For package boogie-friends =================================================
;; (add-to-list 'load-path "~/repos/boogie-friends/emacs/")
;; (require 'boogie-friends)
;; (setq flycheck-dafny-executable "/Users/jorendorff/play/dafny/dafny-3.0.0pre1-prebuilt/dafny")
;; ;;(setq flycheck-boogie-executable "PATH-TO-BOOGIE")  ;; can't figure out
;; (setq flycheck-z3-smt2-executable "/Users/jorendorff/play/dafny/dafny-3.0.0pre1-prebuilt/z3/bin/z3")
;; (setq flycheck-inferior-dafny-executable "/Users/jorendorff/play/dafny/dafny-3.0.0pre1-prebuilt/dafny-server")
;; ;;(setq boogie-friends-profile-analyzer-executable "PATH-TO-Z3-AXIOM-PROFILER") ;; Optional


;; For package multiple-cursors ===============================================
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; Custom.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-comment-prefix-regexp "#\\|//+>?\\|\\**")
 '(create-lockfiles nil)
 '(fill-column 99)
 '(flycheck-checker-error-threshold 2000)
 '(global-company-mode nil)
 '(grep-find-command '("rg -n -H --no-heading -e '' " . 27))
 '(grep-use-null-device nil)
 '(haskell-mode-hook '(turn-on-haskell-indentation))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(lean-memory-limit 4096)
 '(lean-rootdir "~/.elan")
 '(magit-list-refs-sortby '("-creatordate"))
 '(magit-repository-directories '(("/Users/jorendorff/src/blackbird" . 0)))
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))
 '(package-selected-packages
   '(wgsl-mode rg rustic magit git-commit magit-section transient with-editor lua-mode terraform-mode use-package play-rust yasnippet lsp-treemacs protobuf-mode company lsp-ui flycheck lsp-mode yaml-mode nix-mode magithub markup-faces adoc-mode deft flymake-go go-mode proof-general company-lean helm-lean lean-mode xah-math-input boogie-friends idris-mode clojure-mode markdown-mode zoom-frm haskell-mode cl-lib))
 '(paren-match-face 'paren-face-match-light)
 '(paren-sexp-mode t)
 '(pop-up-windows nil)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((eval c-set-offset 'arglist-cont-nonempty
           '(c-lineup-gcc-asm-reg c-lineup-arglist))
     (eval c-set-offset 'arglist-close 0)
     (eval c-set-offset 'arglist-intro '++)
     (eval c-set-offset 'case-label 0)
     (eval c-set-offset 'statement-case-open 0)
     (eval c-set-offset 'substatement-open 0)
     (buffer-file-coding-system . utf-8-unix)
     (insert-tabs-mode)))
 '(sentence-end-double-space nil)
 '(vc-handled-backends '(Git)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t nil)))
 '(lsp-rust-analyzer-inlay-face ((t (:inherit nil :background "gray98" :foreground "gray68")))))
