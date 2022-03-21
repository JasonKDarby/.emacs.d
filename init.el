(setq package-enable-at-startup nil)

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

(setq straight-use-package-by-default t)

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

;; Turn on line numberscoun
(global-display-line-numbers-mode 0)

;; Hey, it's a nice looking font
(set-face-attribute
 'default nil
 :font "Fantasque Sans Mono")

(set-default-coding-systems 'utf-8)

;; Delete selected text with what you type
(delete-selection-mode 1)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(setq tramp-default-method "ssh")

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

(mapc 'global-unset-key [[up] [down] [left] [right] [prior] [next] [home] [end] [insert]])

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; END emacs things not related to specific packages


;; THEMES - I am fickle
;; See visualizations at https://github.com/doomemacs/themes/tree/screenshots
(use-package doom-themes)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

;; See visualizations at https://belak.github.io/base16-emacs/
(use-package base16-theme)


(use-package dired-single
  :defer t)

(use-package dired-collapse
  :defer t)

(use-package dired-ranger
  :defer t)

(use-package diminish)

(use-package paredit
  :hook ((clojure-mode . paredit-mode)
         (cider-mode . paredit-mode)
         (cider-repl-mode . paredit-mode)
         (emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (scheme-mode . paredit-mode))
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (show-paren-mode t))

(use-package paredit-everywhere
  :config
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

(use-package indent-guide
  :config
  (indent-guide-global-mode))

(use-package company
  :config
  ;; Makes autocomplete return uppercase if the completion calls for it
  (setq company-dabbrev-downcase 0)

  ;; Set autocomplete to trigger immediately
  (setq company-idle-delay 0)

  (global-company-mode))

;; BEGIN clojure
(use-package clojure-mode)

(use-package cider
  :config
  ;; Disabled due to incompatibility with nREPL (why??)
  ;; TODO: fix
  ;;(setq cider-print-fn "pprint")

  ;; Prevent cider from showing the error buffer automatically
  (setq cider-show-error-buffer nil)

  ;; Prevent cider from jumping to the error buffer
  (setq cider-auto-select-error-buffer nil)

  ;; A function for starting cider with a profile selected
  ;; See https://stackoverflow.com/questions/18304271/how-do-i-choose-switch-leiningen-profiles-with-emacs-nrepl
  (defun start-cider-repl-with-profile ()
    (interactive)
    (letrec ((profile (read-string "Enter profile name: "))
             (lein-params (concat "with-profile +" profile " repl :headless")))
      (message "lein-params set to: %s" lein-params)
      (set-variable 'cider-lein-parameters lein-params)
      (cider-jack-in '()))))

(setq lsp-lens-enable t)

;; https://clojure-lsp.io/clients/#emacs
(use-package lsp-mode
  :hook ((clojure-mode . lsp)
         (clojurec-mode . lsp)
         (clojurescript-mode . lsp))
  :config
  (dolist (m '(clojure-mode
               clojurec-mode
               clojurescript-mode
               clojurex-mode))
    (add-to-list 'lsp-language-id-configuration `(,m . "clojure"))))

(use-package lsp-ui
  :commands lsp-ui-mode)

;; END clojure

(use-package json-mode
  :defer 20
  :custom (json-reformat:indent-width 2)
  :mode ()
  :bind
  (:package json-mode-map
            :map json-mode-map
            ("C-c <tab>" . json-mode-beautify)))

(use-package diminish
  :config
  (diminish 'eldoc-mode))

(use-package rainbow-delimiters
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (cider-mode . rainbow-delimiters-mode)
         (cider-repl-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode))
  :config (add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode))

(use-package magit
  :bind ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-filenotify)

(use-package magit-find-file
  :bind ("C-c f" . magit-find-file-completing-read))

(use-package magit-todos
  :defer t)

;; ESHELL
(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))

;; See https://config.daviwil.com/emacs#better-completions-with-ivy
(use-package ivy
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
	       ("TAB" . ivy-alt-done)
	       ("C-f" . ivy-alt-done)
	       ("C-l" . ivy-alt-done)
	       ("C-j" . ivy-next-line)
	       ("C-k" . ivy-previous-line)
	       ;; binds go here
	       :map ivy-switch-buffer-map
	       ("C-k" . ivy-previous-line)
	       ("C-l" . ivy-done)
	       ("C-d" . ivy-switch-buffer-kill)
	       :map ivy-reverse-i-search-map
	       ("C-k" . ivy-previous-line)
	       ("C-d" . ivy-reverse-i-search-kill))
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
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
	       ("C-x b" . counsel-ibuffer)
	       ("C-x C-f" . counsel-find-file)
	       ("C-M-j" . counsel-switch-buffer)
	       ("C-M-l" . counsel-imenu)
	       :map minibuffer-local-map
	       ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package counsel-projectile
  :after projectile
  :bind (("C-M-p" . counsel-projectile-find-file))
  :config
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
  :config
  (global-aggressive-indent-mode 1))

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
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package eldoc-mode
  :straight nil
  :hook ((clojure-mode . eldoc-mode)
         (cider-mode . eldoc-mode)
         (cider-repl-mode . eldoc-mode)
         (emacs-lisp-mode . eldoc-mode))
  :config
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package disable-mouse
  :config
  (global-disable-mouse-mode))



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
