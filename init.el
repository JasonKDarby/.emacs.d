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
    all-the-icons-ivy
    zoom
    aggressive-indent
    powerline
    base16-theme
    mwim))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

(defvar mode-hooks
  '(clojure-mode-hook
    cider-mode-hook
    cider-repl-mode-hook
    emacs-lisp-mode-hook
    eval-expression-minibuffer-setup-hook))

(defvar modes
  '(rainbow-delimiters-mode
    paredit-mode
    eldoc-mode
    zoom-mode
    aggressive-indent-mode
    projectile-mode))

;; Apply modes to mode-hooks
(dolist (hook mode-hooks)
  (dolist (mode modes)
    (add-hook hook mode)))

;; Make the cursor a horizontal bar instead of the box
(set-default 'cursor-type 'hbar)

;; Autocomplete everywhere.  It's kind of funny, it autocompleted most of this
;; line.
(global-company-mode)

;; Makes autocomplete return uppercase if the completion calls for it.
(setq company-dabbrev-downcase 0)

;; Set autocomplete to trigger immediately
(setq company-idle-delay 0)

;; Controls the cider pretty print function
(setq cider-pprint-fn "pprint")

;; Prevent cider from showing the error buffer automatically
(setq cider-show-error-buffer nil)

;; Prevent cider from jumping to the error buffer
(setq cider-auto-select-error-buffer nil)

;; Set ivy related settings and keybindings
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

(setq magit-completing-read-function 'ivy-completing-read)

(setq projectile-completion-system 'ivy)

;; Define theme
(load-theme 'base16-chalk t)

;; TODO:  What does this even do?  Find out and remove if unnecessary.
(setq ansi-color-faces-vector
      [default default default italic underline success warning error])

;; [MWIM](https://github.com/alezost/mwim.el) allows home and end to move to the
;; beginning/end of expressions on a line in addition to the beginning/end of
;; the line itself.

;; Set home for keyboards with home keys
(global-set-key [home] 'mwim-beginning)

;; Set C-a because my macbook keyboard doesn't have easy home
(global-set-key "\C-a" 'mwim-beginning)

;; Set end for keyboards with home keys
(global-set-key [end] 'mwim-end)

;; Set C-e because my macbook keyboard doesn't have easy home
(global-set-key "\C-e" 'mwim-end)

;; Set font, will probably need to update with fallback options
(set-face-attribute 'default nil
                    :family (cond
                             ((eq system-type 'darwin) "Fira Code Retina")
                             (t "Source Code Pro Regular"))
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Apply syntax highlighting to defined symbols
(setq cider-font-lock-dynamically '(macro core function var))

;; Delete selected text with what you type
(delete-selection-mode 1)

;; Make backspace and delete perform expected functionality, paredit changes it
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-forward-delete 'delete-selection 'supersede)

;; Prevent start screen
(setq inhibit-startup-screen t)

;; Sets the appearance of the bar that shows up at the bottom of buffers
(require 'powerline)
(powerline-center-theme)

;; See https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; Make emacs put backup files outside of the current directory
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; Backup settings that will hopefully help me out
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Set syntax highlighting at 80 characters
;; See https://www.emacswiki.org/emacs/EightyColumnRule
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

(custom-set-variables
 '(zoom-size '(0.618 . 0.618)))
