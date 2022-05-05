;;; init.el --- Emacs initialization
;;
;;; Commentary:
;; This is my personal configuration, primarily for Clojure development on macos.
;; It should work on Linux and Windows as well.
;;
;; Some modes require external dependencies.
;; - doc-view-mode https://www.emacswiki.org/emacs/DocViewMode
;; - flycheck-clj-kondo https://github.com/borkdude/flycheck-clj-kondo
;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vanilla Emacs configuration

(defvar using-macp
  (eq 'darwin system-type))

(defvar using-windowsp
  (eq 'windows-nt system-type))

;; Note my keybinds are meant for the Kinesis Advantage 2.

(when using-macp
  (setq mac-command-modifier 'control
        mac-option-modifier      'meta
        mac-right-option-modifier 'hyper
        mac-right-command-modifier 'super
        mac-control-modifier 'hyper))

(when using-windowsp
  (setq w32-rwindow-modifier 'hyper)

  ;; Emacs doesn't differentiate left/right ctrl for windows so this depends on right ctrl
  ;; being rebound to f13.  It doesn't actually simulate the super keypress but it uses the
  ;; effect.  I'm not sure if there's a practical or actual difference.
  (define-key key-translation-map (kbd "<f13>") 'event-apply-super-modifier)

  (w32-register-hot-key [H-]))

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Delete by moving to trash instead of just poof
(setq-default delete-by-moving-to-trash t)

;; Make all backup files go to one spot instead of littering folders everywhere
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))

;; Disable menu bar
(menu-bar-mode -1)

;; Remove tool bar and scroll bar
(tool-bar-mode -1)

(scroll-bar-mode -1)

;; Automatically sync changes from filesystem to buffer (if buffer has no unsaved changes)
(global-auto-revert-mode 1)

;; Disable the startup screen
(setq inhibit-startup-message t)

;; Turn off line numberscount
(global-display-line-numbers-mode 0)

;; Hey, it's a nice looking font
(set-face-attribute
 'default nil
 :font "Fantasque Sans Mono"
 :height (cond (using-macp 160)
               (using-windowsp 130))
 :weight 'normal)

;; Delete selected text with what you type
(delete-selection-mode 1)

;; Make ESC quit prompts
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; This is on probation to break habits of escaping windows
(global-set-key (kbd "<escape>") 'keyboard-quit)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "<f6>") 'xref-find-definitions)

(defvar tramp-default-method "ssh")

;; Default indent size to 2 spaces
(setq-default tab-width 2)
;; Use spaces instead of tabs for indentation
(setq-default indent-tabs-mode nil)

;; Control buffer placement https://config.daviwil.com/emacs#control-buffer-placement
(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

(setq even-window-sizes nil)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; See https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

;; https://www.emacswiki.org/emacs/WindMove
;; Shift+arrow will move to buffers directionally
;; Note that this works even if arrow keys are unbound (because it's a modifier + key)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; https://emacs.stackexchange.com/a/5604
;; Allows assigning a copy target in dired based on other open dired buffers.
;; Try opening a folder, then another folder, then C on a directory to move.
(defvar dired-dwim-target 1)

(defvar dired-kill-when-opening-new-dired-buffer 1)

(setq dired-listing-switches "-alh")

(global-prettify-symbols-mode 1)

;; https://emacs.stackexchange.com/a/9411
(setq-default ediff-forward-word-function 'forward-char)

(setq read-minibuffer-restore-windows nil)

;; Useful functions
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; put auto generated custom information into a separate file
(setq custom-file "~/.emacs.d/custom.el")
(cond ((file-directory-p custom-file)
       (load custom-file))
      (t (progn
           (write-region "" nil custom-file)
           (load custom-file))))

;; company-specific-file should contain a function named
;; `apply-company-specific-configuration' that takes `PACKAGE'
;; as an argument and executes company specific configuration.
(setq company-specific-file "~/.emacs.d/company-specific.el")
(cond ((file-exists-p company-specific-file)
       (load company-specific-file))
      (t (message "No company specific configuration found.")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management setup

(setq package-enable-at-startup nil)
(defvar straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configuration

(use-package dash)

;; Packages to only use when using macos
(when using-macp
  (use-package launchctl))

(use-package csv-mode)

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C--" . er/contract-region)))

(use-package undo-tree
  :custom
  (undo-tree-visualizer-diff t)
  ;; TODO: this should be dynamically constructed based on user-emacs-directory
  ;; but some detail of list evaluation causes an error because the args fail stringp
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  (undo-tree-visualizer-timestamps t)
  :config
  (global-undo-tree-mode))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . markdown-view-mode)
  :init (setq markdown-command "multimarkdown"))

;; THEMES - I am fickle
;; See visualizations at https://github.com/doomemacs/themes/tree/screenshots
(use-package doom-themes)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

;; See visualizations at https://belak.github.io/base16-emacs/
(use-package base16-theme)

(use-package dired-subtree
  :defer t
  :bind ([remap dired-maybe-insert-subdir] . dired-subtree-cycle))

(use-package dired-filter
  :defer t
  :init
  (setq dired-filter-group-saved-groups '(("default"
                                           ("Directories"
                                            (directory))
                                           ("Clojure"
                                            (extension "clj" "cljs" "cljc" "edn")))))
  :hook ((dired-mode . dired-filter-group-mode))
  :custom
  (dired-filter-prefix "H-d f"))

(use-package dired-narrow
  ;; Needs to come after dired-filter so the / keybind isn't overwritten
  :after dired-filter
  :defer t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package diminish)

(use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (clojurescript-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (inf-clojure-mode . paredit-mode)
         (inf-clojure-minor-mode . paredit-mode)
         (lisp-data-mode . paredit-mode))
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (show-paren-mode t))

(use-package indent-guide
  :config
  (indent-guide-global-mode))

(use-package company
  :hook (prog-mode . company-mode)
  :bind (("<backtab>" . company-complete-common))
  :config
  ;; Makes autocomplete return uppercase if the completion calls for it
  (setq company-dabbrev-downcase 0)

  ;; Set company settings according to https://github.com/clojure-lsp/clojure-lsp/issues/884 for performance
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.2))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package yasnippet-snippets)

;; BEGIN clojure

(use-package flycheck-clj-kondo
  :config
  (global-flycheck-mode))

(use-package clojure-mode
  :bind (
         :map clojure-mode-map
         ("\C-c\C-k" . 'reload-current-clj-ns)
         ("\C-cl" . 'inf-clojure-erase-buffer)
         ("\C-c\C-tn" . 'inf-clojure-run-tests-in-ns)
         ("\C-c\C-t\C-n" . 'inf-clojure-run-tests-in-ns)
         ("\C-c\C-tt" . 'inf-clojure-run-current-test)
         ("\C-c\C-t\C-t" . 'inf-clojure-run-current-test))
  :config
  (setq clojure-indent-style 'always-align)
  (require 'flycheck-clj-kondo))

;; See https://martintrojer.github.io/clojure/2015/02/14/clojure-and-emacs-without-cider-redux
(use-package inf-clojure
  :hook ((clojure-mode . inf-clojure-minor-mode)
         ;; Prevents emacs from becoming unresponsive with giant lines.
         ;; ...But at what cost?
         (inf-clojure-mode . visual-line-mode))
  :bind (
         :map inf-clojure-mode-map
         ("\C-cl" . 'inf-clojure-erase-buffer)
         ("{" . #'paredit-open-curly)
         ("}" . #'paredit-close-curly)
         :map inf-clojure-minor-mode)
  :config
  ;; This is the default, not sure why you wouldn't want it to be read-only.
  (setq inf-clojure-prompt-read-only t)

  ;; Use compliment https://github.com/alexander-yakushev/compliment for clojure completions
  (inf-clojure-update-feature 'clojure 'completion "(compliment.core/completions \"%s\")")

  (inf-clojure-update-feature 'clojure 'arglists "(try (:arglists (clojure.core/meta (clojure.core/resolve
(clojure.core/read-string \"%s\")))) (catch Throwable t nil))")

  ;; prevent company-mode from freezing Emacs when the REPL is busy
  ;; (add-hook 'inf-clojure-minor-mode-hook (lambda () (setq completion-at-point-functions nil)))

  (defun reload-current-clj-ns (next-p)
    (interactive "P")
    (let ((ns (clojure-find-ns)))
      (message (format "Loading %s ..." ns))
      (inf-clojure-eval-string (format "(do (require '%s :reload) (in-ns '%s))" ns ns))))

  (defun inf-clojure-erase-buffer ()
    (interactive)
    (with-current-buffer (get-buffer "*inf-clojure*")
      (erase-buffer))
    (inf-clojure-eval-string ""))

  (defun inf-clojure-run-current-test ()
    (interactive)
    (pcase-let ((`(,declaration-macro ,symbol-name) (clojure-find-def)))
      (let ((ns (clojure-find-ns)))
        (if (string= declaration-macro "deftest")
            (progn (message (format "Running test %s/%s ..." ns symbol-name))
                   (inf-clojure-eval-string (format
                                             "(do (use 'clojure.test) (clojure.test/test-vars [#'%s/%s]))"
                                             ns symbol-name ns)))
          (message (format "%s/%s was created using %s, not deftest" ns symbol-name declaration-macro))))))
  
  (defun inf-clojure-run-tests-in-ns ()
    (interactive)
    (let ((ns (clojure-find-ns)))
      (message (format "Running tests in %s ..." ns))
      (inf-clojure-eval-string (format "(do (use 'clojure.test) (run-tests '%s))" ns))))

  (defun inf-clojure-toggle-log-activity ()
    (interactive)
    (setq inf-clojure-log-activity (not inf-clojure-log-activity)))

  (apply-company-specific-configuration 'inf-clojure))

;; END clojure

(use-package dumb-jump
  :config
  ;; This does not work with use-package :hook, I think because it registers it as
  ;; xref-backend-functions-hook.
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package json-mode
  :defer 20
  :custom (json-reformat:indent-width 2)
  :bind
  (:package json-mode-map
            :map json-mode-map
            ("C-c <tab>" . json-mode-beautify)))

(use-package rainbow-delimiters
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (inf-clojure-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)
         (eval-expression-minibuffer-setup . rainbow-delimiters-mode)))

(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-find-file
  :bind ("C-c f" . magit-find-file-completing-read))

(use-package magit-todos
  :defer t)

;; Allows walking through changes
(use-package git-timemachine)

;; Provides links to remote repos
(use-package git-link)

(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

;; ESHELL
(use-package exec-path-from-shell
  :config
  (when using-macp
    (setq ls-lisp-use-insert-directory-program nil)
    (require 'ls-lisp)

    (setq shell-command-switch "-ic")

    (exec-path-from-shell-copy-env "JAVA_HOME")
    (exec-path-from-shell-copy-env "VISUAL")
    (exec-path-from-shell-copy-env "EDITOR")
    (exec-path-from-shell-initialize)))

(use-package with-editor
  :demand t
  ;; TODO: running these commands results in with-editor trying to be invoked as a function but is void
  :bind (([remap async-shell-command] . 'with-editor-async-shell-command)
         ([remap shell-command] . 'with-editor-shell-command)))

(use-package eshell-git-prompt)

(use-package eshell
  :requires with-editor
  :hook ((eshell-mode . 'with-editor-export-editor)
         (eshell-mode . 'with-editor-export-git-editor))
  :config
  (eshell-git-prompt-use-theme 'git-radar))

(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

;; See https://config.daviwil.com/emacs#better-completions-with-ivy
(use-package ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)
         ("H-s" . swiper-thing-at-point)
         :map ivy-minibuffer-map
	       ("TAB" . ivy-alt-done)
	       ;; binds go here
	       :map ivy-switch-buffer-map
	       :map ivy-reverse-i-search-map)
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t))

;; Make sure to run (all-the-icons-install-fonts) and install the fonts to your system!
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :after counsel)

(use-package projectile
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode)
  (apply-company-specific-configuration 'projectile)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
	       ("C-x b" . counsel-ibuffer)
	       ("C-x C-f" . counsel-find-file)
         ([remap bookmark-jump] . counsel-bookmark)
	       ("C-M-j" . counsel-switch-buffer)
	       ("C-M-l" . counsel-imenu)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)
         ("C-S-o" . counsel-rythmbox)
         ("<f6>" . ivy-resume)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> o" . counsel-describe-symbol)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("M-y" . counsel-yank-pop)
         ("s-s" . counsel-projectile-ag)
	       :map minibuffer-local-map
	       ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil) ;; Don't start searches with ^
  (setq counsel-projectile-ag-initial-input '(ivy-thing-at-point))) ;; Prefill ag with thing-at-point

(use-package counsel-projectile
  :after projectile
  :config
  (setq counsel-projectile-switch-project-action #'counsel-projectile-switch-project-action-dired)
  (counsel-projectile-mode))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (clojure-mode . aggressive-indent-mode)
         (inf-clojure-mode . aggressive-indent-mode))
  :config
  ;; A temporary fix to prevent messages from being logged to the buffer as of Emacs 28.
  ;; See https://github.com/Malabarba/aggressive-indent-mode/pull/148
  (setq aggressive-indent-region-function #'(lambda (x y) (let ((inhibit-message t)) (indent-region x y)))))

;; Contains functions for moving to the beginning/end of line
(use-package mwim
  :bind (("C-a" . 'mwim-beginning)
         ("C-e" . 'mwim-end)))

;; Set syntax highlighting at 80 characters
(use-package whitespace
  :config
  (setq whitespace-style '(face empty tabs lines-trail trailing))
  (global-whitespace-mode 0))

(use-package which-key
  :bind
  ([remap describe-mode] . which-key-show-major-mode)
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode))

(use-package hackernews)

(use-package disable-mouse
  :config
  (global-disable-mouse-mode))

;;; init.el ends here
