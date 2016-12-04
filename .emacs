(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(column-number-mode t)
(show-paren-mode t)

;; Enable company globally for all modes
;;(global-company-mode)
;;
;; Reduce the time after which the company autocomplete popup opens
;;(setq company-idle-delay 0.2)
;;
;; Reduce the number of characters before company kicks in
;;(setq company-minimum-prefix-length 1)


;; zoom-frm.el
(require 'zoom-frm)
(define-key ctl-x-map [(control ?+)] 'zoom-in/out)
(define-key ctl-x-map [(control ?-)] 'zoom-in/out)
(define-key ctl-x-map [(control ?=)] 'zoom-in/out)
(define-key ctl-x-map [(control ?0)] 'zoom-in/out)

;; Set path to racer binary


(add-to-list 'load-path "~/dev/dotfiles/.emacs.d/my-site-lisp")
(add-to-list 'load-path "~/dev/mozilla-elisp")

(tool-bar-mode 0)  ;; no nasty tool-bar!

(visit-tags-table "~/dev/gecko/TAGS")

(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

;; blessed silence
(setq ring-bell-function 'ignore)

;; mouse-6 is triggered by two-finger-scrolling to the right; mouse-7 to the left
(global-set-key [mouse-6] (function (lambda () (interactive) nil)))
(global-set-key [mouse-7] (function (lambda () (interactive) nil)))

(require 'vc-hg)
(require 'page-ext)

;; Structural editing ftw! Thanks Scot!
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)

(push "/opt/local/share/emacs/site-lisp" load-path)
(autoload 'gid "idutils" nil t)

;; orgmode.org told me to put these lines in
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))



;; commented out because broken
;; (require 'polymode)
;; (require 'poly-markdown)
;;
;; use this to customize pm-inner/markdown?
;; (defcustom rustbook-pm-inner/markdown
;;   (pm-hbtchunkmode-auto "markdown"
;;                      :head-reg "^[ \t]*```\\(?:\\)[{ \t]*\\w.*$"
;;                      :tail-reg "^[ \t]*```[ \t]*$"
;;                      :retriever-regexp "```[ \t]*\\(?:{\\|lang=\\)?\\(\\(\\w\\|\\s_\\)*\\)"
;;                      :font-lock-narrow t))
;;
;; (defcustom rustbook-pm-poly/markdown
;;   (pm-polymode-multi-auto "markdown"
;;                         :hostmode 'pm-host/markdown
;;                         :auto-innermode 'rustbook-pm-inner/markdown
;;                         :init-functions '(poly-markdown-remove-markdown-hooks))
;;   "Markdown custom configuration for rustbook"
;;   :group 'polymodes
;;   :type 'object)
;;
;; (define-polymode rustbook-poly-markdown-mode rustbook-pm-poly/markdown)
;; (add-to-list 'auto-mode-alist '("\\.md" . rustbook-poly-markdown-mode))

;; yay rust
;;(add-to-list 'load-path "~/dev/rust/src/etc/emacs/")
;;(require 'rust-mode)
(add-hook 'rust-mode-hook
          (function (lambda ()
                      (setq-local electric-indent-chars (append '(?} ?\;) electric-indent-chars)))))

;; Save before grepping, thanks to Jim Blandy.
(defadvice grep (before save-before-grepping)
  (save-some-buffers))
(ad-activate 'grep)

;; Steve Yegge's js2-mode
;;(autoload 'js2-mode "js2" nil t)
;;(add-to-list 'auto-mode-alist '("\\.[ej]sm?$" . js2-mode))

;; Cycle through grep hits with C-`.
(global-set-key [?\C-`] 'next-error)
(global-set-key [?\C-~] 'previous-error)
(global-set-key "\M-gr" 'grep)
(global-set-key "\M-q"  'fill-paragraph)

(defun select-all ()
  (interactive "")
  (set-mark (point-min))
  (goto-char (point-max)))
(global-set-key [(super ?a)] 'select-all)

(defun js-debug ()
  (interactive "")
  (gud-gdb "gdb --fullname --args /home/jorendorff/dev/gecko/js/src/d-objdir/dist/bin/js -f /home/jorendorff/dev/gecko/js/src/tmp.js"))

(global-set-key [(super ?g)] 'gud-gdb)
(global-set-key [(super shift ?g)] 'js-debug)
(global-set-key [(super ?b)] 'gud-break)
(global-set-key [(super ?s)] 'gud-step)
(global-set-key [(super ?n)] 'gud-next)
(global-set-key [(super ?f)] 'gud-finish)
(global-set-key [(super ?c)] 'gud-cont)
(global-set-key [(super ?r)] 'gud-run)
;; todo: [(super ?k)] to kill debuggee

;; C-< and C->
(defun indent-rigidly-4 (start end)
  (interactive "r")
  (indent-rigidly start end 4))
(global-set-key [?\C->] 'indent-rigidly-4)
(defun dedent-rigidly-4 (start end)
  (interactive "r")
  (indent-rigidly start end -4))
(global-set-key [?\C-<] 'dedent-rigidly-4)

;; Wheeeee!
(server-start)

;; Compile with F7

(defun string-ends-with-p (str prefix)
  (let ((strlen (length str))
	(prelen (length prefix)))
    (and (>= strlen prelen)
	 (string= (substring str (- strlen prelen) strlen)
		  prefix))))

(defun in-spidermonkey-dir-p ()
  (let ((buf (current-buffer)))
    (and buf
	 (let ((file-name (buffer-file-name buf)))
	   (and file-name
		(string-ends-with-p (file-name-directory file-name)
				    "/js/src/"))))))

(defun set-compile-command ()
    (if (in-spidermonkey-dir-p)
	(setq compile-command "~/dev/dotfiles/myscripts/js-build")))

(add-hook 'change-major-mode-hook 'set-compile-command)

(global-set-key [f7] 'compile)

;; Nicety for ViewSourceWith.
(add-hook 'text-mode-hook
	  (function
	   (lambda ()
	     (setq fill-column 72))))


;; "Adobe style"
(defun string-match-p (regexp str)
  (not (null (string-match regexp str))))

(defconst adobe-tab-width 4)

(defun javascript-code-hook ()
  (setq indent-tabs-mode nil))

(add-hook 'js-mode-hook (function javascript-code-hook))

(defun adobe-code-hook ()
  ;; Install adobe-c++ style if not already installed...
  (progn (c-set-style "stroustrup")
         (setq indent-tabs-mode nil)))

(add-hook 'c-mode-common-hook (function adobe-code-hook))

;; AAAAAAAAAARRRGH x-select-enable-clipboard t

(defun unix ()
  "Change the current buffer to use Unix line endings."
  (interactive)
  (set-buffer-file-coding-system 'unix))



(defconst dev-directory (concat (getenv "HOME") "/dev"))

(defun review ()
  (interactive)
  ;; Copy current buffer to new buffer $DEV/reviews/review-$FILENAME.txt
  (let* ((patch (buffer-string))
         (original-basename (car (last (split-string (buffer-file-name) "/"))))
         (file (concat dev-directory "/reviews/review-" original-basename ".txt")))
    (find-file file)
    (if (file-exists-p file) (error "Review file already exists"))
    (insert patch))

  ;; Now hack up the patch.
  (let ((first-line (point-min))
        (last-line (save-excursion (goto-char (point-max)) (forward-line 0) (point))))
    (string-rectangle first-line last-line "    >"))
  (replace-regexp "^    >diff" "\n    >diff" nil (point-min) (point-max))
  (replace-regexp "^    >@@" "\n    >@@" nil (point-min) (point-max))
  (save-buffer)
  (goto-char (point-min)))

(defun review-prune ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ;; merge "-x\n+x\n" to " x\n", repeatedly
    ;; "now you have two problems" :-|
    (while (re-search-forward "\\(^    >[^-].*\n\\)    >-\\(.*\\)\n\\(\\(    >-.*\n\\)*\\)    >\\+\\2\n" nil t)
      (let ((p (match-beginning 0)))
	(replace-match "\\1    > \\2\n\\3" nil nil)
	(goto-char p)
	(forward-line -1)))

    ;; remove changeless hunks
    (goto-char (point-min))
    (while (search-forward-regexp "^\n    >@.*\n\\(    > .*\n\\)+\n" nil t)
      (replace-match "\n")
      (forward-line -2))))

(define-key global-map "\C-c\C-a" 'review-prune)

(defun jimb-diff-mode-hook ()
  (define-key diff-mode-map "\M-q" nil))
(add-hook 'diff-mode-hook 'jimb-diff-mode-hook)

(setq org-default-notes-file (concat dev-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)


;; HTML entities (key bindings chosen to match MacOS defaults)

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

(global-set-key (kbd "C-x g") 'magit-status)

;; Custom.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-comment-prefix-regexp "#\\|//+>?\\|\\**")
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(fill-column 79)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(package-selected-packages
   (quote
    (magit markdown-mode markdown-mode+ zoom-frm racer paredit haskell-mode flycheck-rust erlang elixir-mode company-racer color-theme-solarized cider)))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(pop-up-windows nil)
 '(safe-local-variable-values
   (quote
    ((js-indent-level . 2)
     (js2-basic-offset . 4)
     (js2-strict-trailing-comma-warning)
     (js2-skip-preprocessor-directives . t)
     (js2-basic-offset . 2)
     (buffer-file-coding-system . utf-8-unix)
     (insert-tabs-mode))))
 '(vc-handled-backends nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#fdf6e3" :foreground "#657b83" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 85 :width normal :foundry "unknown" :family "VL Gothic")))))

(defun save-macro (name)
  "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file user-init-file)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

(fset 'js-try-result-to-bool
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([tab 74 83 95 84 82 89 95 82 69 83 85 76 84 95 84 79 95 66 79 79 76 4 4 4 right 99 120 44 32 4 134217749 134217742 59 67108896 right 5 right left 23] 0 "%d")) arg)))

