(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(defvar my-packages
  '(better-defaults
    projectile
    clojure-mode
    cider
    rainbow-delimiters
    paredit
    paredit-everywhere
    company
    magit
    magit-filenotify
    magit-find-file
    counsel
    counsel-projectile
    ivy
    ivy-pages
    swiper
    all-the-icons-ivy))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(setq cider-font-lock-dynamically '(macro core function var))

(set-default 'cursor-type 'hbar)

(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)

(add-hook 'clojure-mode-hook #'paredit-mode)

(add-hook 'cider-mode-hook #'eldoc-mode)

(add-hook 'cider-repl-mode-hook #'paredit-mode)

(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(global-company-mode)

(setq company-dabbrev-downcase 0)

(setq company-idle-delay 0)

(setq cider-pprint-fn "pprint")

(setq projectile-completion-system 'ivy)

(setq magit-completing-read-function 'ivy-completing-read)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(load-theme 'tango-dark t)

(setq ansi-color-faces-vector [default default default italic underline success warning error])
