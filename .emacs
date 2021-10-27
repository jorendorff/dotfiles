;; .emacs - finally got this just how i like it

;; Package configuration ======================================================

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)  ;; auto-install packages, YOLO

(use-package flycheck)
(use-package lsp-mode)
(use-package lsp-ui)
(use-package magit)
(use-package multiple-cursors)
(use-package markdown-mode)
(use-package protobuf-mode)
(use-package yaml-mode)
(use-package lean-mode)
(use-package company-lean)
(use-package helm-lean)
(use-package haskell-mode)

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

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'before-save-hook (lambda () (when (eq 'rust-mode major-mode)
                                           (lsp-format-buffer)))))


;; Other junk =================================================================

(add-to-list 'load-path "~/src/boogie-friends/emacs/")


;; Disable ns-popup-font-panel on Mac
(global-unset-key (kbd "s-t"))

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


;;(visit-tags-table "~/work/gecko/TAGS")

;; (require 'etags-select)
;; (global-set-key "\M-?" 'etags-select-find-tag-at-point)
;; (global-set-key "\M-." 'etags-select-find-tag)

(defun gcr/plist-to-alist (ls)
  "Convert a plist to an alist. Primarily for old color-theme themes."
  (let ((result nil))
    (while ls
      (add-to-list 'result (cons (car ls) (cadr ls)))
      (setq ls (cddr ls)))
    result))
(defalias 'plist-to-alist 'gcr/plist-to-alist)

;; mouse-6 is triggered by two-finger-scrolling to the right; mouse-7 to the left
(global-set-key [mouse-6] (function (lambda () (interactive) nil)))
(global-set-key [mouse-7] (function (lambda () (interactive) nil)))

;; Don't vanish and lose keyboard focus when I type Ctrl-Z, the standard key
;; for undo on Linux and Windows.
(define-key global-map [(control ?z)] nil)
(define-key ctl-x-map [(control ?z)] nil)

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


;; My custom tweaks ===========================================================

;; Show column numbers in mode line.
(column-number-mode t)

;; When cursor is on a bracket, highlight the matching bracket.
(show-paren-mode t)

;; No nasty tool-bar! good grief
(tool-bar-mode 0)

;; Save before grepping, thanks to Jim Blandy.
(defadvice grep (before save-before-grepping)
  (save-some-buffers))
(ad-activate 'grep)

;; Cycle through grep hits with C-`.
(global-set-key [?\C-`] 'next-error)
(global-set-key [?\C-~] 'previous-error)
(global-set-key "\M-gr" 'grep)
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


;; For package zoom-frm =======================================================
;; (global-set-key (kbd "C-x C-+") 'zoom-in/out)
;; (global-set-key (kbd "C-x C-=") 'zoom-in/out)
;; (global-set-key (kbd "C-x C--") 'zoom-in/out)
;; (global-set-key (kbd "C-x C-0") 'zoom-in/out)
;;
;; (define-key ctl-x-map [(control ?+)] 'zoom-in/out)
;; (define-key ctl-x-map [(control ?-)] 'zoom-in/out)
;; (define-key ctl-x-map [(control ?=)] 'zoom-in/out)
;; (define-key ctl-x-map [(control ?0)] 'zoom-in/out)
;;
;; (define-key global-map [(super ?+)] 'zoom-in/out)
;; (define-key global-map [(super ?=)] 'zoom-in/out)
;; (define-key global-map [(super ?-)] 'zoom-in/out)
;; (define-key global-map [(super ?0)] 'zoom-in/out)


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

;; I don't know what this is ==================================================
(require 'seq)

;; Extremely complicated hack attempting to make lsp-diagnostis useful ========
;; why does everything have to be garbage, i don't know
(defun jorendorff-goto-first-error ()
    (interactive)
  (let* ((file-diagnostics-list-list-pairs (ht-items (lsp-diagnostics)))
         (file-diagnostics-list-pairs (seq-mapcat (function (lambda (pair)
                                                          (let ((file (car pair))
                                                                (diag-list-list (cdr pair)))
                                                            (mapcar (function (lambda (message-list)
                                                                                (cons file message-list)))
                                                                    diag-list-list))))
                                              file-diagnostics-list-list-pairs))
         (file-diagnostic-pairs (seq-mapcat (function (lambda (pair)
                                                    (let ((file (car pair))
                                                          (diag-list (cdr pair)))
                                                      (mapcar (function (lambda (diag)
                                                                          (cons file diag)))
                                                              diag-list))))
                                            file-diagnostics-list-pairs))
         (file-diag-pairs-sorted (seq-sort (function (lambda (a b) (<= (ht-get (cdr a) "severity")
                                                                       (ht-get (cdr b) "severity"))))
                                           file-diagnostic-pairs))
         (first-file-diag-pair (car file-diag-pairs-sorted)))
    (when first-file-diag-pair
      (let* ((file (car first-file-diag-pair))
             (diag (cdr first-file-diag-pair))
             (range (ht-get diag "range"))
             (start (ht-get range "start"))
             (line (ht-get start "line"))
             (col (ht-get start "character")))
        (find-file file)
        (goto-line line)
        (forward-char col)))))

(global-set-key (kbd "C-x C-`") 'jorendorff-goto-first-error)


;; ============================================================================

;; Change silly defaults for Home and End keys on Mac
(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)

;; Mac-like key bindings: Cmd+A to select all
(defun select-all ()
  (interactive "")
  (set-mark (point-min))
  (goto-char (point-max)))
(global-set-key [(super ?a)] 'select-all)

;; Mac-like key-bindings: Cmd+G to find again.
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

;; Mac-like key-bindings: Cmd+W to close a buffer.
(defun kill-current-buffer ()
  "Kill the current buffer without prompting which buffer to kill."
  (interactive)
  (kill-buffer))
(global-set-key (kbd "s-w") 'kill-current-buffer)

;; C-tab to switch buffers.
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)  ;; linux :-|
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)  ;; mac :-\

;; Special stuff to avoid when running in a terminal
(when (not (null (window-system)))
  (server-start)
  (setq confirm-kill-emacs 'yes-or-no-p))


;; Neat functions I will never use again ======================================

(defun gen-password ()
  "Insert a randomly generated password into the buffer at point."
  (interactive)
  (insert (shell-command-to-string
           "python3 -c 'import secrets; print(secrets.token_urlsafe(12))'")))

(defun jorendorff-lean-arrange-windows ()
  "Arrange windows for lean-mode."
  (interactive)
  (lean-ensure-info-buffer lean-show-goal-buffer-name)
  (lean-ensure-info-buffer lean-next-error-buffer-name)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (switch-to-buffer lean-show-goal-buffer-name)
  (split-window-below)
  (other-window 1)
  (switch-to-buffer lean-next-error-buffer-name)
  (other-window 1))

(defun grep-for-symbol-at-point ()
  "Do a grep for the symbol currently under the cursor"
  (interactive)
  (let* ((cur-word (thing-at-point 'symbol))
         (cmd (concat "grep -rnH " cur-word " .")))
    (grep-apply-setting 'grep-command cmd)
    (grep cmd)))


;; Custom.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-comment-prefix-regexp "#\\|//+>?\\|\\**")
 '(create-lockfiles nil)
 '(fill-column 79)
 '(flycheck-checker-error-threshold 2000)
 '(global-company-mode nil)
 '(grep-command "git-grep-rs -nH ")
 '(grep-use-null-device nil)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(lean-memory-limit 4096)
 '(lean-rootdir "~/.elan")
 '(lsp-keymap-prefix "s-l")
 '(lsp-rust-all-features t)
 '(lsp-rust-analyzer-cargo-watch-command "clippy")
 '(lsp-rust-analyzer-display-chaining-hints t)
 '(lsp-rust-analyzer-display-parameter-hints t)
 '(lsp-rust-analyzer-server-display-inlay-hints t)
 '(lsp-rust-clippy-preference "on")
 '(lsp-rust-features [])
 '(lsp-rust-server (quote rust-analyzer))
 '(lsp-signature-render-documentation nil)
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-sideline-show-hover t)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(package-selected-packages
   (quote
    (use-package play-rust yasnippet lsp-treemacs protobuf-mode company lsp-ui flycheck lsp-mode yaml-mode nix-mode magithub markup-faces adoc-mode deft flymake-go go-mode proof-general company-lean helm-lean lean-mode xah-math-input boogie-friends multiple-cursors idris-mode clojure-mode markdown-mode zoom-frm rust-mode magit haskell-mode cl-lib)))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(pop-up-windows nil)
 '(ring-bell-function (quote ignore))
 '(safe-local-variable-values
   (quote
    ((eval c-set-offset
           (quote arglist-cont-nonempty)
           (quote
            (c-lineup-gcc-asm-reg c-lineup-arglist)))
     (eval c-set-offset
           (quote arglist-close)
           0)
     (eval c-set-offset
           (quote arglist-intro)
           (quote ++))
     (eval c-set-offset
           (quote case-label)
           0)
     (eval c-set-offset
           (quote statement-case-open)
           0)
     (eval c-set-offset
           (quote substatement-open)
           0)
     (buffer-file-coding-system . utf-8-unix)
     (insert-tabs-mode))))
 '(sentence-end-double-space nil)
 '(vc-handled-backends nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t nil)))
 '(lsp-rust-analyzer-inlay-face ((t (:inherit nil :background "gray98" :foreground "gray68")))))
