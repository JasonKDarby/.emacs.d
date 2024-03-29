;;; init.el --- Emacs initialization
;;
;;; Commentary:
;; This is my personal configuration, primarily for Clojure development on macos.
;; It should work on Linux and Windows as well.
;;
;; Some modes require external dependencies.
;; - doc-view-mode https://www.emacswiki.org/emacs/DocViewMode
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
  (progn
    (setq mac-command-modifier 'control
          mac-option-modifier      'meta
          mac-right-option-modifier 'hyper
          mac-right-command-modifier 'super
          mac-control-modifier 'hyper)
    ;; https://www.reddit.com/r/emacs/comments/xfhnzz/weird_errors_with_latest_build_of_emacs/
    ;; This can probably be removed in a future emacs upgrade
    (customize-set-variable 'native-comp-driver-options '("-Wl,-w"))))

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

;; Always highlight the current line.  This is only marginally useful for me but is immensely useful for viewers when screen sharing.
(global-hl-line-mode)

;; Hey, it's a nice looking font
(set-face-attribute
 'default nil
 :font "Fantasque Sans Mono"
 :height (cond (using-macp 160)
               (using-windowsp 130))
 :weight 'normal)

;; We need to make sure the font gets evaluated per frame or frames started
;; by emacsclient won't load the font while starting emacs directly will.
(add-to-list 'default-frame-alist
             '(font . "Fantasque Sans Mono"))

;; Delete selected text with what you type
(delete-selection-mode 1)

;; Space between lines
(setq line-spacing 0.2)

;; Make ESC quit prompts
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; This is on probation to break habits of escaping windows
(global-set-key (kbd "<escape>") 'keyboard-quit)

(global-set-key (kbd "M-o") 'other-window)

(global-set-key (kbd "<f6>") 'xref-find-definitions)

(global-set-key (kbd "s-<backspace>") 'delete-region)

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
(defun jdarb/remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun jdarb/change-theme (theme)
  (if (eq (car custom-enabled-themes) theme)
      (disable-theme theme)
    (enable-theme theme)))

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

;; Provides useful elisp functions, primarily a dependency for other packages.
(use-package dash)

;; Packages to only use when using macos
(when using-macp
  (use-package launchctl))

;; Packages to only use when using windows
(when using-windowsp
  (use-package powershell))

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
;; favorites:
;; base16-chalk
;; doom-city-lights
;; doom-miramare
;; gruvbox
;; doom-moonlight
;; base16-bespin
;; base16-eighties
;; base16-mocha
;; doom-peacock
(use-package doom-themes)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-light-medium t))

;; See visualizations at https://belak.github.io/base16-emacs/
(use-package base16-theme)

(use-package dired-subtree
  :defer t
  :bind ([remap dired-maybe-insert-subdir] . dired-subtree-cycle))

(use-package dired-filter
  :defer t
  :init
  (setq dired-filter-group-saved-groups '(("default"
                                           ("emacs"
                                            (extension "el"))
                                           ("directories"
                                            (directory))
                                           ("clojure"
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
  :hook ((clojure-mode           . paredit-mode)
         (clojurescript-mode     . paredit-mode)
         (emacs-lisp-mode        . paredit-mode)
         (lisp-mode              . paredit-mode)
         (lisp-interaction-mode  . paredit-mode)
         (scheme-mode            . paredit-mode)
         (inf-clojure-mode       . paredit-mode)
         (inf-clojure-minor-mode . paredit-mode)
         (lisp-data-mode         . paredit-mode))
  :bind (:map paredit-mode-map
              ("RET" . nil))
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (show-paren-mode t))

(use-package indent-guide
  :config
  (indent-guide-global-mode))

(use-package company
  :hook ((prog-mode . company-mode)
         (inf-clojure-mode . company-mode))
  :bind (("<backtab>" . company-complete-common))
  :config
  ;; Makes autocomplete return uppercase if the completion calls for it
  (setq company-dabbrev-downcase 0)

  ;; Set company settings according to https://github.com/clojure-lsp/clojure-lsp/issues/884 for performance
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.2))

;; BEGIN clojure

(use-package clojure-mode
  :hook ((clojure-mode . display-line-numbers-mode))
  :bind (
         :map clojure-mode-map
         ("\C-c\C-k"     . 'reload-current-clj-ns)
         ("\C-cl"        . 'inf-clojure-erase-buffer)
         ("\C-c\C-tn"    . 'inf-clojure-run-tests-in-ns)
         ("\C-c\C-t\C-n" . 'inf-clojure-run-tests-in-ns)
         ("\C-c\C-tt" . 'inf-clojure-run-current-test)
         ("\C-c\C-t\C-t" . 'inf-clojure-run-current-test)
         :map clojurescript-mode-map
         ("\C-c\C-k" . 'reload-current-cljs-ns))
  :config
  (setq clojure-indent-style 'always-align))

;; See https://martintrojer.github.io/clojure/2015/02/14/clojure-and-emacs-without-cider-redux
(use-package inf-clojure
  :straight (inf-clojure :type git :host github :repo "JasonKDarby/inf-clojure")
  :hook ((clojure-mode . inf-clojure-minor-mode)
         ;; Prevents emacs from becoming unresponsive with giant lines.
         ;; ...But at what cost?
         (inf-clojure-mode . visual-line-mode))
  :bind (
         :map inf-clojure-mode-map
         ("\C-cl"    . 'inf-clojure-erase-buffer)
         ("{"        . #'paredit-open-curly)
         ("}"        . #'paredit-close-curly)
         ("\C-x\s-e" . 'inf-clojure-rebl-inspect)
         :map inf-clojure-minor-mode)
  :config
  ;; This is the default, not sure why you wouldn't want it to be read-only.
  (setq inf-clojure-prompt-read-only t)

  ;; Let eglot handle this
  (setq inf-clojure-enable-eldoc nil)

  ;; Use compliment https://github.com/alexander-yakushev/compliment for clojure completions
  ;; Eglot provides completions in clojure/clojurec/clojurescript modes but this is needed for repl completions.
  (inf-clojure-update-feature 'clojure 'completion "(compliment.core/completions \"%s\")")

  ;; Make this cljs compatible for when I use a repl that upgrades to cljs
  (inf-clojure-update-feature 'clojure 'arglists "(try (-> (symbol \"%s\" #?(:clj (->> str clojure.core/read-string clojure.core/resolve clojure.core/meta :arglists) :cljs (->> cljs.core/resolve cljs.core/meta :arglists))) #?(:clj (catch Throwable _ nil) :cljs (catch :default _ nil))))")

  (defun jdarb/buffer-substring-last-sexp ()
    (buffer-substring-no-properties (save-excursion (backward-sexp) (point)) (point)))

  (defun inf-clojure-rebl-inspect ()
    (interactive)
    (inf-clojure-eval-string (format "(cognitect.rebl/inspect %s)\n" (jdarb/buffer-substring-last-sexp))))
  
  (defun reload-current-clj-ns (next-p)
    (interactive "P")
    (let ((ns (clojure-find-ns)))
      (message (format "Loading %s..." ns))
      (inf-clojure-eval-string
       (inf-clojure--forms-without-newlines
        (format "(do (use '[clojure.tools.namespace.repl :only (refresh)])(refresh)(in-ns '%s))" ns)))))

  (defun reload-current-cljs-ns (next-p)
    (interactive "P")
    (let ((ns (clojure-find-ns)))
      (message (format "Loading %s..." ns))
      (inf-clojure-eval-string (inf-clojure--forms-without-newlines (format "(in-ns '%s)" ns)))))

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
            (progn (message (format "Running test %s/%s..." ns symbol-name))
                   (inf-clojure-eval-string (format
                                             "(do (use 'clojure.test)(clojure.test/test-vars [#'%s/%s]))"
                                             ns symbol-name)))
          (message (format "%s/%s was created using %s, not deftest" ns symbol-name declaration-macro))))))
  
  (defun inf-clojure-run-tests-in-ns ()
    (interactive)
    (let ((ns (clojure-find-ns)))
      (message (format "Running tests in %s..." ns))
      (inf-clojure-eval-string (format "(do (use 'clojure.test) (run-tests '%s))" ns))))

  (defun inf-clojure-toggle-log-activity ()
    (interactive)
    (setq inf-clojure-log-activity (not inf-clojure-log-activity)))

  (defun inf-clojure-print-last-exception ()
    (interactive)
    (inf-clojure-eval-string "(do (use 'clojure.stacktrace) (print-stack-trace *e 5))"))

  (alist-get 'apropos (alist-get 'combined-clj-cljs inf-clojure-repl-features))
  
  (let ((clojure-repl-features (alist-get 'clojure inf-clojure-repl-features))
        (cljs-repl-features (alist-get 'cljs inf-clojure-repl-features)))
    (add-to-list 'inf-clojure-repl-features
                 `(combined-clj-cljs . ((doc . "(-> %s #?(:default clojure.repl/doc :cljs cljs.repl/doc))")
                                        (source . "(-> %s #?(:clj clojure.repl/source :cljs cljs.repl/source))")
                                        (arglists . "(try (-> '%s #?(:clj (->> str clojure.core/read-string clojure.core/resolve clojure.core/meta :arglists) :cljs (->> cljs.core/resolve cljs.core/meta :arglists))) #?(:clj (catch Throwable _ nil) :cljs (catch :default _ nil)))")
                                        (apropos  . ,(concat "(let [inf-clojure-dummy-val (str %s)] #?(:clj "
                                                             (string-replace "\"%s\"" "inf-clojure-dummy-val" (alist-get 'apropos clojure-repl-features))
                                                             " :cljs "
                                                             (string-replace "\"%s\"" "inf-clojure-dummy-val" (alist-get 'apropos cljs-repl-features))
                                                             ")"))
                                        (ns-vars . "(-> %s #?(:clj clojure.repl/dir :cljs cljs.repl/dir))")
                                        (set-ns . "(in-ns '%s)")
                                        (macroexpand . "(-> '%s #?(:clj clojure.core/macroexpand :cljs cljs.core/macroexpand))")
                                        (macroexpand-1 . "(-> '%s #?(:clj clojure.core/macroexpand-1 :cljs cljs.core/macroexpand-1))"))))

    ;; It should be possible to get cljs completion via suitable depending on project setup but dependably enough to set it by default.
    (inf-clojure-update-feature 'combined-clj-cljs 'completion "#?(:clj (compliment.core/completions \"%s\") :cljs nil)"))

  (when (featurep 'company-specific)
    (apply-company-specific-configuration 'inf-clojure)))

(use-package eglot
  :ensure t
  :bind (("C-S-<tab>" . 'eglot-code-actions))
  :config
  (setq eglot-confirm-server-initiated-edits nil)
  
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(inf-clojure-mode . ("clojure-lsp"))))
  
  ;; Make sure eglot loads automatically in clojurescript-mode
  (add-hook 'clojurescript-mode-hook 'eglot-ensure)
  (add-hook 'clojurec-mode-hook 'eglot-ensure)
  (add-hook 'clojure-mode-hook 'eglot-ensure))

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
  :hook ((clojure-mode                     . rainbow-delimiters-mode)
         (inf-clojure-mode                 . rainbow-delimiters-mode)
         (emacs-lisp-mode                  . rainbow-delimiters-mode)
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
         ([remap shell-command]       . 'with-editor-shell-command)))

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
  (when (featurep 'company-specific)
    (apply-company-specific-configuration 'projectile))
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel
  :demand t
  :bind (("M-x"                 . counsel-M-x)
	       ("C-x b"               . counsel-ibuffer)
	       ("C-x C-f"             . counsel-find-file)
         ([remap bookmark-jump] . counsel-bookmark)
	       ("C-M-j"               . counsel-switch-buffer)
	       ("C-M-l"               . counsel-imenu)
         ("C-c g"               . counsel-git)
         ("C-c j"               . counsel-git-grep)
         ("C-c k"               . counsel-ag)
         ("C-x l"               . counsel-locate)
         ("C-S-o"               . counsel-rythmbox)
         ("<f6>"                . ivy-resume)
         ("<f1> f"              . counsel-describe-function)
         ("<f1> v"              . counsel-describe-variable)
         ("<f1> o"              . counsel-describe-symbol)
         ("<f1> l"              . counsel-find-library)
         ("<f2> i"              . counsel-info-lookup-symbol)
         ("<f2> u"              . counsel-unicode-char)
         ("M-y"                 . counsel-yank-pop)
         ("s-s"                 . counsel-projectile-rg)
	       :map minibuffer-local-map
	       ("C-r"                 . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil) ;; Don't start searches with ^
  (setq counsel-projectile-ag-initial-input '(ivy-thing-at-point)) ;; Prefill ag with thing-at-point
  (setq counsel-projectile-rg-initial-input '(ivy-thing-at-point)) ;; Prefill rg with thing-at-point
  )

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
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode    . aggressive-indent-mode)
         (clojure-mode       . aggressive-indent-mode)
         (clojure-mode       . aggressive-indent-mode)
         (clojurec-mode      . aggressive-indent-mode)
         (clojurescript-mode . aggressive-indent-mode)
         (inf-clojure-mode   . aggressive-indent-mode))
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

;; Shows whitespace at the end of lines.
;; It's real uggo so don't leave trailing whitespace.
(setq-default show-trailing-whitespace t)

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
