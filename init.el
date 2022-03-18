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

;; Enable menu bar
(menu-bar-mode 1)

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

;; END emacs things not related to specific packages

;; See visualizations at https://github.com/doomemacs/themes/tree/screenshots
(use-package doom-themes
  :config
  (load-theme 'doom-manegarm t))

(use-package dired-single
  :defer t)

(use-package dired-collapse
  :defer t)

(use-package dired-ranger
  :defer t)

(use-package diminish)

(use-package paredit
  :diminish paredit-mode
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
  :diminish indent-guide-mode
  :config
  (indent-guide-global-mode))

(use-package company
  :diminish company-mode
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
  :diminish rainbow-delimiters-mode
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
  :diminish
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
  :diminish projectile-mode
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
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode 1))

;; Contains functions for moving to the beginning/end of line
(use-package mwim
  :bind (("C-a" . 'mwim-beginning)
         ("C-e" . 'mwim-end)))

;; Set syntax highlighting at 80 characters
(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style '(face empty tabs lines-trail trailing))
  (global-whitespace-mode 0))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package powerline
  :config
  (powerline-center-theme))

(use-package eldoc-mode
  :straight nil
  :hook ((clojure-mode . eldoc-mode)
         (cider-mode . eldoc-mode)
         (cider-repl-mode . eldoc-mode)
         (emacs-lisp-mode . eldoc-mode))
  :config
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

;; Useful functions
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;; TODO: make this go to a different file
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("58115a5caf2c19aa48de6562c70dfaec47b429707389e03ef59f22f6f6be65ab" "4294559fea7408018ed1879bd921d79729ef45a65ed4045dc5d9dbcd89e5725f" "5acb6c8cb656d2c5beeda835a7870f95792a08efbb7011a0f40d5123158cef73" "c162e6033d801080f0af776ef829cbc08cfff649bab72ca5be1122939ce3eff2" "fc8567bceceacb43b80e0bf28ba709500c139f3a389f575ca77503621a820bee" "1072d0e743f32dbf63de967331bca80f30edb00c3eda2d65a63b3e60d029c479" "ec1e5d8a563f3b89215dde47e45f61bf58b4efc7385750b6235ddf9232da06b4" "9dbf8bbe18f69ca7f9cddf3f78c45ae804935b1412e719b1ce3f08a0701bfb95" "8e660fd9166774764d69a4c19a86822f656abaacf6584531b2a01949452e441f" "329458d9b4c24c8c253ab2fd57bc48a1a38eb0fbd7d5f3f3f923cbb1530af87e" "868d20268275edd9bde186f2c5e2d790ee0961f8f30566e42ae94829a460d6c8" "9ad49a343bd4a455cf53b3349b51cb87e52d1271d98cc2141803801cbfdfca0f" "3d39868703fc81340f7b7bcf184980689496893a5f075db3ebf71806d9b3e9d7" "59f15086eeccce2d6a650492ba0124cb4bedaab20810d12a6b81a191ec4b9d9b" "b44396213fcedc2e9677814fd02111b346a181c9864a1909002c6829d6fbd570" "52294a82f187b6a9d601a26b491ebd7adf14f24c3b95867f3c35a1b2e794af2b" "bace3bc543a9966d962766c08567a8b15890c44cc7bf2ea93ce20d99795f7fb5" "d6c3b3665814aa428b655a272cd0d278fb9d3d40239dfb56d5ee3bc82868d4e0" "a7a91004284dbf68f35c4befb6d40609457f7d93a856aaccb16e25d761adcf55" "9e469516b728ddf4f86d49a400924bc6b78adb0947f728978a0402e64b15dfe1" "d42fb53fc081b2258e8bd1a3ed6f46a911c5f0e596c3a99218ba85b84a908cb3" "167171ae09d9161a27bfc1a4b0812a0590469b91583f60bb49f30ae29ce0515e" "d8f84f55ebc7233b4657c2db1f5a82bcd4f99dca5e41c6c7c4c2bc45ccfab10b" "ecec5199cd5685333f51e12473ed703d7437c2d87db4cec8543ce4684c5d374e" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
