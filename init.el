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
    base16-theme))

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

;; Autocomplete everywhere.  It's kind of funny, it autocompleted most of this line.
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
(setq ansi-color-faces-vector [default default default italic underline success warning error])

;; https://stackoverflow.com/questions/145291/smart-home-in-emacs/145359
;; TODO:  Make shift+beginning of line select passed over text.
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

;; Set home for keyboards with home keys
(global-set-key [home] 'smart-beginning-of-line)

;; Set C-a because my macbook keyboard doesn't have easy home
(global-set-key "\C-a" 'smart-beginning-of-line)

;; Set font, will probably need to update with fallback options
(set-face-attribute 'default nil
                    :family "Source Code Pro Regular"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Apply syntax highlighting to defined symbols
(setq cider-font-lock-dynamically '(macro core function var))

;; Delete selected text with what you type
(delete-selection-mode 1)

;; Prevent start screen
(setq inhibit-startup-screen t)

;; Sets the appearance of the bar that shows up at the bottom of buffers
(require 'powerline)
(powerline-center-theme)
