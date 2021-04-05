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

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package company
  :config
  ;; Makes autocomplete return uppsercase if the completion calls for it
  (setq company-dabbrev-downcase 0)

  ;; Set autocomplete to trigger immediately
  (setq company-idle-delay 0)

  (global-company-mode))

(use-package projectile
  :config
  (projectile-global-mode))

;; BEGIN clojure
(use-package clojure-mode)

(use-package cider
  :config
  (setq cider-pprint-fn "pprint")

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

(use-package py-autopep8)

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

(use-package rainbow-delimiters)

(use-package paredit
  :config
  ;;Make backspace and delete perform expected functionality
  (put 'paredit-backward-delete 'delete-selection 'supersede)
  (put 'paredit-forward-delete 'delete-selection 'supersede))

(use-package paredit-everywhere)

(use-package magit)

(use-package magit-filenotify)

(use-package magit-find-file)

(use-package counsel
  :bind (:map minibuffer-local-map)
  :config
  (counsel-mode 1))

(use-package counsel-projectile)

(use-package swiper)

(use-package ivy
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map)
  :config
  (ivy-mode 1))

(use-package all-the-icons-ivy)

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
(use-package mwim)

(use-package base16-theme
  :config
  (load-theme 'base16-chalk t))

;; Set syntax highlighting at 80 characters
(use-package whitespace
  :config
  (setq whitespace-style '(face empty tabs lines-trail trailing))
  (global-whitespace-mode 1))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package powerline
  :config
  (powerline-center-theme))

;; Set non-global modes

(defun apply-modes-to-hooks (modes hooks)
  (dolist (hook hooks)
    (dolist (mode modes)
      (add-hook hook mode))))

(defvar lisp-modes
  '(rainbow-delimiters-mode
    eldoc-mode))

(defvar lisp-mode-hooks
  '(clojure-mode-hook
    cider-mode-hook
    cider-repl-mode-hook
    emacs-lisp-mode-hook
    eval-expression-minibuffer-setup-hook))

;; Startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (cd default-directory)
            (eshell)))
