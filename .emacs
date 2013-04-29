(column-number-mode t)
(show-paren-mode t)

(add-to-list 'load-path "~/.emacs.d/my-site-lisp")
(add-to-list 'load-path "~/dev/mozilla-elisp")

(require 'etags-select)
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)


(require 'color-theme-solarized)
(color-theme-solarized-light)

(require 'vc-hg)
;;(require 'mercurial-queues)
(require 'page-ext)

(push "/opt/local/share/emacs/site-lisp" load-path)
(autoload 'gid "idutils" nil t)

;; orgmode.org told me to put these lines in
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; yay rust
(add-to-list 'load-path "~/dev/rust/src/etc/emacs/")
(require 'rust-mode)

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

;; C-< and C->
(defun indent-rigidly-4 (start end)
  (interactive "r")
  (indent-rigidly start end 4))
(global-set-key [?\C->] 'indent-rigidly-4)
(defun dedent-rigidly-4 (start end)
  (interactive "r")
  (indent-rigidly start end -4))
(global-set-key [?\C-<] 'dedent-rigidly-4)

(defun whip ()
  (interactive "")
  (select-frame (make-frame))
  (set-background-color "papaya whip"))

(defun blue ()
  (interactive "")
  (select-frame (make-frame))
  (set-background-color "#e0ecf8"))

(defun reset-frames ()
  (let ((old-frames (frame-list))
	(new-frames '(((width . 80) (height . 150)))))
    (dolist (params new-frames) (make-frame params))
    (dolist (f old-frames) (delete-frame f))))

(if (display-mouse-p)
    (reset-frames))

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
	(setq compile-command "~/bin/js-build")))

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

(defun adobe-code-hook ()
  ;; Install adobe-c++ style if not already installed...
  (if (not (assoc "adobe-c++" c-style-alist))
      (setq c-style-alist
	    (cons
	     `("adobe-c++" "stroustrup"
	       (c-basic-offset . ,adobe-tab-width)
	       (indent-tabs-mode . t))
	     c-style-alist)))
  ;; Now enable whichever C++ style is appropriate for this code.
  (let ((chunk (buffer-substring 1 (+ 1 (min (buffer-size) 1500)))))
    (if (or (string-match-p "Adobe System Inc" chunk)
	      (string-match-p "Adobe AS3" chunk))
	(progn (c-set-style "adobe-c++")
	       (setq tab-width adobe-tab-width))
      (progn (c-set-style "stroustrup")
	     (setq indent-tabs-mode nil)))))

(add-hook 'c-mode-common-hook (function adobe-code-hook))

;; AAAAAAAAAARRRGH x-select-enable-clipboard t

(defun unix ()
  "Change the current buffer to use Unix line endings."
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defconst dev-directory "/Users/jorendorff/dev")

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

(defun insert-ldquo () (interactive) (insert "&ldquo;"))
(define-key global-map "\M-[" 'insert-ldquo)

(defun insert-rdquo () (interactive) (insert "&rdquo;"))
(define-key global-map "\M-{" 'insert-rdquo)

(defun insert-lsquo () (interactive) (insert "&lsquo;"))
(define-key global-map "\M-]" 'insert-lsquo)

(defun insert-rsquo () (interactive) (insert "&rsquo;"))
(define-key global-map "\M-}" 'insert-rsquo)

(defun insert-mdash () (interactive) (insert "&mdash;"))
(define-key global-map "\M-_" 'insert-mdash)



;; Custom.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(fill-column 79)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (1 ((shift) . 5) ((control)))))
 '(paren-match-face (quote paren-face-match-light))
 '(paren-sexp-mode t)
 '(pop-up-windows nil)
 '(safe-local-variable-values (quote ((js-indent-level . 2) (js2-basic-offset . 4) (js2-strict-trailing-comma-warning) (js2-skip-preprocessor-directives . t) (js2-basic-offset . 2) (buffer-file-coding-system . utf-8-unix) (insert-tabs-mode))))
 '(vc-handled-backends nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((default (:height 100 :family "Monaco")) (nil nil))))
