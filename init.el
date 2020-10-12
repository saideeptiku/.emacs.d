;; init.el --- Emacs configuration
;; Saideeep's Configuration for Emacs
;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    elpy ;; add elpy package
    flycheck
    py-autopep8
    material-theme
    spacemacs-theme
    counsel
    smex
    neotree
    which-key
    doom-themes
    rainbow-delimiters
    company-jedi
    company-quickhelp
    markdown-mode
    sphinx-doc
    ein
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; -------------------------------------------------------------------
;; BASIC CUSTOMIZATION
;; ------------------------------------------------------------------



;; change location of autosave files
(setq auto-save-file-name-transforms
        `((".*" "~/.emacs.d/emacs-saves/" t)))


;; enable and set modes
;;(setq visible-bell t) ;; disable the hideous sound that is the bell
(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t) ;; hide the startup message
(global-linum-mode t) ;; enable line numbers globally
(menu-bar-mode -1) ;; disable menu-bar
(tool-bar-mode -1) ;; disable tool=bar
(ivy-mode 1) ;; enable ivy
(electric-pair-mode 1) ;; enable pairing of brackets etc.
(show-paren-mode 1) ;; highlight matching brackets
(which-function-mode 1) ;; show current function name
(which-key-mode 1) ;; enable which-key, shows future key combinate
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; mode settings
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 100
                    :weight 'semi-light
                    :width 'normal)
(setq-default cursor-type 'box) ;; set cursor to var type
(set-cursor-color "#9247ed") (blink-cursor-mode 0) ;; stop the cursor from blinking
(toggle-scroll-bar -1) ;; disable scrollbar

;; ------------------------------------------------------------------
;; WHICH-KEY SETTINGS
;; ------------------------------------------------------------------
;; location of which-key window. valid values: top, bottom, left, right,
;; or a list of any of the two. If it's a list, which-key will always try
;; the first location first. It will go to the second location if there is
;; not enough room to display any keys in the first location
(setq which-key-side-window-location 'bottom)

;; max width of which-key window, when displayed at left or right.
;; valid values: number of columns (integer), or percentage out of current
;; frame's width (float larger than 0 and smaller than 1)
(setq which-key-side-window-max-width 0.33)

;; max height of which-key window, when displayed at top or bottom.
;; valid values: number of lines (integer), or percentage out of current
;; frame's height (float larger than 0 and smaller than 1)
(setq which-key-side-window-max-height 0.25)

;; skip checking package signature;
;; do you even know what you are doing
(setq package-check-signature nil)
;;-------------------------------------------------------------------
;; SPACEMACS THEME
;;------------------------------------------------------------------
;;(load-theme 'spacemacs-dark t) ;; load material theme

;; ------------------------------------------------------------------
;; DOOM THEME CONFIG
;; ------------------------------------------------------------------
(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-molokai t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
;; (doom-themes-neotree-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; -------------------------------------------------------------------
;; PYTHON CONFIGURATION
;; -------------------------------------------------------------------

;; enable ivy
(elpy-enable)

;; profile elpy
;; use elp-results to view results for profiler
(elp-instrument-package "elpy-")

;; force python3 RPC; why? cuz its awesome!
(setq elpy-rpc-python-command "python3")

;; ipython3 when you do 'run-python'
(setq python-shell-interpreter "ipython"
    python-shell-interpreter-args "--simple-prompt -i --pprint")
;; (when (executable-find "ipython")
;;   (setq python-shell-interpreter "python"))

(defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;; it seems like elpy is asking for doc too often
;; https://github.com/jorgenschaefer/elpy/issues/1287
;; wait 0.5 seconds before asking for doc
(setq eldoc-idle-delay 0.2)

;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; run flycheck on save only
(setq flycheck-check-syntax-automatically '(save mode-enable))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


(let ((workon-home (expand-file-name "~/.local/share/virtualenvs/")))
  (setenv "WORKON_HOME" workon-home)
  (setenv "VIRTUALENVWRAPPER_HOOK_DIR" workon-home))

(add-hook 'python-mode-hook (lambda ()
			      (require 'sphinx-doc)
			      (sphinx-doc-mode t)))
;;-----------------------------------------------------------------------------
;; hl-todo setup
;;-----------------------------------------------------------------------------
(add-hook 'prog-mode-hook (lambda () (hl-todo-mode 1)))

(setq hl-todo-highlight-punctuation ":"
		   hl-todo-keyword-faces
		   `(("TODO"       warning bold)
		     ("FIXME"      error bold)
		     ("HACK"       font-lock-constant-face bold)
		     ("REVIEW"     font-lock-keyword-face bold)
		     ("NOTE"       success bold)
		     ("DEPRECATED" font-lock-doc-face bold)))
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------
;; COMPANY COMPLETE
;;-----------------------------------------------------------------------------
(company-mode 1) ;; company mode should be enable all the time!
;; use C-M-i for autocomplete 
;; (global-set-key (kbd "C-SPC") 'nil)
;; (global-set-key (kbd "C-SPC") 'company-complete-common)
(company-quickhelp-mode 1)
(setq company-quickhelp-delay 1.0)
(setq company-quickhelp-max-lines 30)

;; -----------------------------------------------------------------------------
;; CUSTOM KEYBINDINGS
;; -----------------------------------------------------------------------------
;; Load custom functions
(load "~/.emacs.d/my_funcs.el")

(global-set-key (kbd "M-x") 'counsel-M-x) ;; enable counsel m-x by default

;;search using swiper
(global-set-key (kbd "C-s") 'swiper)

;; clean up
(global-set-key (kbd "C-z") nil) ;; disable minimize
(global-set-key (kbd "M-q") nil)
(global-set-key (kbd "C-q") 'keyboard-escape-quit)

;; copy paste fix ups
(global-set-key (kbd "C-w") 'cut-line-or-region) ;; disable minimize
(global-set-key (kbd "M-w") 'copy-line-or-region) ;; save buffer
(global-set-key (kbd "C-z") 'undo) ;; paste text

;; company related
;; C-M-i autocomplete at line is disabled
(global-set-key (kbd "M-n") 'company-select-next) ;; company next
(global-set-key (kbd "M-p") 'company-select-previous) ;; company previous

;; neotree
(global-set-key (kbd "M-q n") 'neotree) ;; neotree show
(global-set-key (kbd "M-q t") 'neotree-toggle) ;; toggle neotree

;; find file
(global-set-key (kbd "M-q f") 'counsel-find-file)

;; python ops
(global-set-key (kbd "M-q g t d") 'elpy-goto-definition-other-window) ;; show definition
(global-set-key (kbd "M-q w o") 'pyvenv-workon) ;; set work environment

;; window ops
(global-set-key (kbd "M-q w s r") 'split-window-right) ;; split window right
(global-set-key (kbd "M-q w s b") 'split-window-below) ;; split window below
(global-set-key (kbd "M-q w k") 'delete-window) ;; close current window
(global-set-key (kbd "M-q b k") 'kill-buffer) ;; close current window

(global-set-key (kbd "M-q <up>") 'windmove-up) ;; select window up
(global-set-key (kbd "M-q <down>") 'windmove-down) ;; select window down
(global-set-key (kbd "M-q <left>") 'windmove-left) ;; select window left
(global-set-key (kbd "M-q <right>") 'windmove-right) ;; select window right

;; ein specific commands; jupyter notebook
(global-set-key (kbd "M-q i g") 'ein:run)
(global-set-key (kbd "M-q i s") 'ein:stop)
(global-set-key (kbd "M-q i i") 'ein:notebook-kernel-interrupt-command-km)
(global-set-key (kbd "M-q i r") 'ein:notebook-restart-session-command-km)
(global-set-key (kbd "M-q i a") 'ein:worksheet-insert-cell-above-km)
(global-set-key (kbd "M-q i b") 'ein:worksheet-insert-cell-below-km)
(global-set-key (kbd "M-q i t") 'ein:worksheet-toggle-cell-type-km)
(global-set-key (kbd "M-q i d d") 'ein:worksheet-delete-cell)
(global-set-key (kbd "M-q i e") 'ein:worksheet-execute-cell-km)


;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:output-area-inlined-images t)
 '(package-selected-packages
   (quote
    (ein hl-todo poly-markdown mmm-mode markdown-mode+ markdown-mode company-quickhelp company-jedi rainbow-delimiters doom-themes which-key neotree counsel spacemacs-theme material-theme py-autopep8 flycheck elpy better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
