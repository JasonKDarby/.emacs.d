

;; BEGIN emacs things not related to specific packages

(require 'package)

(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0)))

(package-initialize)

;; Delete by moving to trash instead of just poof
(setq-default delete-by-moving-to-trash t)

;; Enable menu bar
(menu-bar-mode 1)

;; Remove tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Remove fringes
(fringe-mode 0)

;; Disable the startup screen
(setq inhibit-startup-message t)

;; Turn on line numbers
(global-display-line-numbers-mode 1)

;; Hey, it's a nice looking font
(set-face-attribute
 'default nil
 :font "Fantasque Sans Mono")

;; Delete selected text with what you type
(delete-selection-mode 1)

;; Make the cursor a horizontal bar instead of the box
(set-default 'cursor-type 'hbar)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; END emacs things not related to specific packages

(use-package diminish)

(use-package flycheck
  :ensure t
  :diminish global-flycheck-mode
  :config
  (global-flycheck-mode))

(use-package paredit
  :ensure t)

(use-package paredit-everywhere
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

(use-package company
  :ensure t
  :config
  ;; Makes autocomplete return uppercase if the completion calls for it
  (setq company-dabbrev-downcase 0)

  ;; Set autocomplete to trigger immediately
  (setq company-idle-delay 0)

  (global-company-mode))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config
  (projectile-mode))

;; BEGIN clojure
(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t
  :config
  (setq cider-print-fn "pprint")

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

;; END clojure

;; BEGIN python

(use-package py-autopep8
  :ensure t)

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  (setq elpy-eldoc-show-current-function nil)

  ;; Enable flycheck
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode)

  ;; Enable autopep8
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; End python

(use-package json-mode
  :ensure t
  :defer 20
  :custom (json-reformat:indent-width 2)
  :mode ()
  :bind
  (:package json-mode-map
            :map json-mode-map
            ("C-c <tab>" . json-mode-beautify)))

(use-package diminish
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :diminish rainbow-delimiters-mode)

(use-package magit
  :ensure t)

(use-package magit-filenotify
  :ensure t)

(use-package magit-find-file
  :ensure t)

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :bind (:map minibuffer-local-map)
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :ensure t)

(use-package swiper
  :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map)
  :config
  (ivy-mode 1))

(use-package all-the-icons-ivy
  :ensure t)

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package aggressive-indent
  :ensure t
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode 1))

;; Contains functions for moving to the beginning/end of line
(use-package mwim
  :ensure t)

(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-chalk t))

;; Set syntax highlighting at 80 characters
(use-package whitespace
  :ensure t
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style '(face empty tabs lines-trail trailing))
  (global-whitespace-mode 1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))

(use-package shell-here
  :ensure t)

;; Set non-global modes

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook #'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'rainbow-delimiters-mode)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'cider-mode-hook #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

(add-hook 'clojure-mode-hook #'eldoc-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

;; Useful functions
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
