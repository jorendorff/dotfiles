(require 'package)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/play/boogie-friends/emacs/")

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

(define-key global-map [(super ?+)] 'zoom-in/out)
(define-key global-map [(super ?=)] 'zoom-in/out)
(define-key global-map [(super ?-)] 'zoom-in/out)
(define-key global-map [(super ?0)] 'zoom-in/out)

;; hippie-expand! should try using M-/ for this
;;(define-key global-map [(meta ?\\)] 'hippie-expand)

;; Command to delete the current file from hg.
(defun delete-current-file ()
 (interactive "")
 (shell-command (concat "hg rm " (buffer-name))))  ;; danger!

;; (define-key ctl-x-map [(control ?d)] 'delete-current-file)

(define-key ctl-x-map [?!] 'window-swap-states)



(defun grep-for-symbol-at-point ()
  "Do a grep for the symbol currently under the cursor"
  (interactive)
  (let* ((cur-word (thing-at-point 'symbol))
         (cmd (concat "grep -rnH " cur-word " .")))
    (grep-apply-setting 'grep-command cmd)
    (grep cmd)))



;; Set path to racer binary


;;(add-to-list 'load-path "~/work/dotfiles/.emacs.d/my-site-lisp")
;;(add-to-list 'load-path "~/work/mozilla-elisp")

(tool-bar-mode 0)  ;; no nasty tool-bar!

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

;; blessed silence
(setq ring-bell-function 'ignore)

;; mouse-6 is triggered by two-finger-scrolling to the right; mouse-7 to the left
(global-set-key [mouse-6] (function (lambda () (interactive) nil)))
(global-set-key [mouse-7] (function (lambda () (interactive) nil)))

;; Don't vanish and lose keyboard focus when I type Ctrl-Z, the standard key
;; for undo on Linux and Windows.
(define-key global-map [(control ?z)] nil)
(define-key ctl-x-map [(control ?z)] nil)


(require 'vc-hg)
(require 'page-ext)

;; Structural editing ftw! Thanks Scot!
;; (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;; (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;; (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;; (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;; (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;; (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;; (add-hook 'clojure-mode-hook          #'enable-paredit-mode)
(add-hook 'dafny-mode-hook #'(lambda ()
                               (company-mode 0)
                               (auto-composition-mode 0)
                               (set (make-local-variable 'tab-width) 4)))



(push "/opt/local/share/emacs/site-lisp" load-path)
(autoload 'gid "idutils" nil t)

;; orgmode.org told me to put these lines in
;; (require 'org-install)
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))



(require 'xah-math-input)
(puthash "<" "⟨" xah-math-input-abrvs)
(puthash ">" "⟩" xah-math-input-abrvs)
(puthash "ring" "∘" xah-math-input-abrvs)
(puthash "compose" "∘" xah-math-input-abrvs)
(puthash "check" "✓" xah-math-input-abrvs)
(puthash "<|" "⊲" xah-math-input-abrvs)
(puthash "|>" "⊳" xah-math-input-abrvs)
(puthash "_|_" "⊥" xah-math-input-abrvs)

;; pedantically insist on U+22C5 DOT OPERATOR rather than U+2022 BULLET
;; or, like, U+2219 BULLET OPERATOR, etc. ad nauseam.
(xah-math-input--add-cycle ["*" "⋅" "•" "×"]) ; multiply, times

;; lots of experimentation on whether to leave xah-math-input-mode enabled
;; or just bind the key in global-map, or what
;;(defun markdown-mode-use-xah-math-input ()
;;  (xah-math-input-mode 1))
;;(add-hook 'markdown-mode-hook 'markdown-mode-use-xah-math-input)
(define-key xah-math-input-keymap (kbd "S-SPC") nil)
(define-key xah-math-input-keymap (kbd "M-S-SPC") 'xah-math-input-change-to-symbol)
(global-xah-math-input-mode)
;;(define-key global-map (kbd "M-S-SPC") 'xah-math-input-change-to-symbol)

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
  (gud-gdb "gdb --fullname --args /home/jorendorff/work/gecko/js/src/d-objdir/dist/bin/js -f /home/jorendorff/work/gecko/js/src/tmp.js"))

;; (global-set-key [(super ?g)] 'gud-gdb)
;; (global-set-key [(super shift ?g)] 'js-debug)
;; (global-set-key [(super ?b)] 'gud-break)
;; (global-set-key [(super ?s)] 'gud-step)
;; (global-set-key [(super ?n)] 'gud-next)
;; (global-set-key [(super ?f)] 'gud-finish)
;; (global-set-key [(super ?c)] 'gud-cont)
;; (global-set-key [(super ?r)] 'gud-run)
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
	(setq compile-command "~/work/dotfiles/myscripts/js-build")))

(add-hook 'change-major-mode-hook 'set-compile-command)

(global-set-key [f7] 'compile)

;; Nicety for ViewSourceWith.
(add-hook 'text-mode-hook
	  (function
	   (lambda ()
	     (setq fill-column 72))))


(defconst mozilla-tab-width 4)

(defun javascript-code-hook ()
  (setq indent-tabs-mode nil))

(add-hook 'js-mode-hook (function javascript-code-hook))

(defun mozilla-code-hook ()
  ;; Install mozilla-c++ style if not already installed...
  (progn (c-set-style "mozilla")
         (setq indent-tabs-mode nil)))

(add-hook 'c-mode-common-hook (function mozilla-code-hook))

;; AAAAAAAAAARRRGH x-select-enable-clipboard t

(defun unix ()
  "Change the current buffer to use Unix line endings."
  (interactive)
  (set-buffer-file-coding-system 'unix))


(defconst home-directory (getenv "HOME"))
(defconst work-directory (concat home-directory "/work"))

(defun review ()
  (interactive)
  ;; Copy current buffer to new buffer $DEV/reviews/review-$FILENAME.txt
  (let* ((patch (buffer-string))
         (original-basename (car (last (split-string (buffer-file-name) "/"))))
         (file (concat work-directory "/reviews/review-" original-basename ".txt")))
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

(setq org-default-notes-file (concat work-directory "/notes.org"))
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

(defconst mozilla-c-style
  '((c-basic-offset             . 2)
    (c-offsets-alist            . ((substatement-open . 0)
                                   (case-label        . *)
                                   (statement-case-intro . *)
                                   (member-init-intro . *)
                                   (innamespace       . 0)
                                   (inline-open       . 0)
                                   (func-decl-cont    . 0)))
    (c-echo-syntactic-information-p . t))
  "Mozilla C++ Programming Style")
(c-add-style "mozilla" mozilla-c-style)

;; Erlang goofs.
;; (setq load-path (cons  "/usr/local/lib/erlang/lib/tools-2.6.8/emacs" load-path))
;; (setq erlang-root-dir "/usr/local/lib/erlang")
;; (setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
;; (require 'erlang-start)

;; For package magit.
(global-set-key (kbd "C-x g") 'magit-status)

;; For package zoom-frm.
(global-set-key (kbd "C-x C-+") 'zoom-in/out)
(global-set-key (kbd "C-x C-=") 'zoom-in/out)
(global-set-key (kbd "C-x C--") 'zoom-in/out)
(global-set-key (kbd "C-x C-0") 'zoom-in/out)


;; For package boogie-friends
(require 'boogie-friends)
(setq flycheck-dafny-executable "/Users/jorendorff/play/dafny/dafny-3.0.0pre1-prebuilt/dafny")
;;(setq flycheck-boogie-executable "PATH-TO-BOOGIE")  ;; can't figure out
(setq flycheck-z3-smt2-executable "/Users/jorendorff/play/dafny/dafny-3.0.0pre1-prebuilt/z3/bin/z3")
(setq flycheck-inferior-dafny-executable "/Users/jorendorff/play/dafny/dafny-3.0.0pre1-prebuilt/dafny-server")
;;(setq boogie-friends-profile-analyzer-executable "PATH-TO-Z3-AXIOM-PROFILER") ;; Optional


;; For package multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; For better isearch behavior.
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


;; Change utterly ridiculous defaults for Home and End keys.
(global-set-key [home] 'move-beginning-of-line)
(global-set-key [end] 'move-end-of-line)

(defun kill-current-buffer ()
  "Kill the current buffer without prompting which buffer to kill."
  (interactive)
  (kill-buffer))

(global-set-key (kbd "s-w") 'kill-current-buffer)
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)

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

(when (not (null (window-system)))
  (server-start)
  (setq confirm-kill-emacs 'yes-or-no-p))


;; Custom.

(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
 '(c-comment-prefix-regexp "#\\|//+>?\\|\\**")
 '(create-lockfiles nil)
 '(fill-column 79)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(lean-memory-limit 4096)
 '(lean-rootdir "/Users/jorendorff/.elan")
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(package-selected-packages
   (quote
    (nix-mode magithub markup-faces adoc-mode deft flymake-go go-mode proof-general company-lean helm-lean lean-mode xah-math-input boogie-friends multiple-cursors idris-mode clojure-mode markdown-mode zoom-frm rust-mode magit haskell-mode cl-lib)))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(pop-up-windows nil)
 '(safe-local-variable-values
   (quote
    ((buffer-file-coding-system . utf-8-unix)
     (insert-tabs-mode))))
 '(vc-handled-backends nil))
