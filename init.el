;;; package --- Summary

;;; Commentary:

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(let ((minver "25"))
  (when (version<= emacs-version "25")
    (error "Your Emacs is too old -- this config requires v%s or higer" minver)))


;;;;;;;;;;;;;;;
;; Essential ;;
;;;;;;;;;;;;;;;

;;
;; Start-up options
;;

; Splash Screen
(setq inhibit-splash-screen t
      inhibit-startup-message t)

; Scroll bar, Tool bar, Menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)

; Marking text and Clipboard
(delete-selection-mode t)
(setq select-enable-clipboard t)

; Display Settings
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))
(setq column-number-mode t)

; Indentation
(setq-default tab-width 4
              indent-tabs-mode nil)

; Backup files
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

; Yes and No
(defalias 'yes-or-no-p 'y-or-n-p)

; Key bindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c C-k") 'compile)

; Shell
(setq shell-file-name "zsh")

; Misc
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq ring-bell-function 'ignore)

; Load Local Files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))


;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'init-elpa)
(require 'init-utils)

(maybe-require-package 'use-package)
(eval-when-compile
  (require 'use-package))

(use-package diminish)
(use-package bind-key)
;; (require 'init-gtags)
(require 'init-spaceline)
(require 'init-evil)
(require 'init-flycheck)
(require 'init-helm)
(require 'init-elfeed)
(require 'init-hexo)

;; Org Prerequisites
(use-package visual-fill-column
  :ensure t)
(require 'init-org)

;;
;; Package Configuration (TODO: Move to init-*.el files)
;;

;;(use-package rainbow-mode
;;  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package smex
  :ensure t
  :init
  (smex-initialize)
  :config
  (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-ag-setup))

(use-package ag
  :ensure t
  :config
  (add-hook 'ag-mode-hook
            (lambda ()
              (wgrep-ag-setup)
              (define-key ag-mode-map (kbd "n") 'evil-search-next)
              (define-key ag-mode-map (kbd "N") 'evil-search-previous)))
  (setq ag-executable "/usr/local/bin/ag")
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  (define-key company-active-map [tab] 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

(use-package swiper
  :ensure t
  :commands swiper
  :bind ("C-s" . swiper))

(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'rainbow-mode)
  (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp))

(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package cider
  :ensure t)

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)))

(use-package magit
  :ensure t
  :commands (magit-blame-mode
	     magit-commit
	     magit-diff
	     magit-log
	     magit-status)
  :config
  (use-package evil-magit
    :ensure t)
  (add-to-list 'magit-log-arguments "--no-abbrev-commit")
  (global-set-key (kbd "C-x g") 'magit-status)
  ;(evil-define-key 'normal magit-mode-map (kbd "gb") 'magit-blame)
  ;(evil-define-key 'normal magit-mode-map (kbd "gc") 'magit-commit)
  ;(evil-define-key 'normal magit-mode-map (kbd "gd") 'magit-diff)
  ;(evil-define-key 'normal magit-mode-map (kbd "gl") 'magit-log)
  ;(evil-define-key 'normal magit-mode-map (kbd "gr") 'magit-reflog)
  ;(evil-define-key 'normal magit-mode-map (kbd "gs") 'magit-status)
  ;(evil-define-key 'normal magit-mode-map (kbd "go") 'delete-other-windows)
  ;(define-key magit-mode-map (kbd "j") 'evil-next-visual-line)
  ;(define-key magit-mode-map (kbd "k") 'evil-previous-visual-line)
  ;(define-key magit-mode-map (kbd "l") 'evil-forward-char)
  ;(define-key magit-mode-map (kbd "h") 'evil-backward-char)
  (add-hook 'git-commit-mode-hook 'evil-insert-state))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;
;; Languages ;;
;;;;;;;;;;;;;;;
(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'emmet-mode-hook
	    (lambda ()
	      (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-l") 'emmet-next-edit-point)
	      (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-h") 'emmet-prev-edit-point))))

(use-package markdown-mode
  :ensure t
  :config
  (define-key markdown-mode-map (kbd "C-\\")  'markdown-insert-list-item)
  (define-key markdown-mode-map (kbd "C-c 1") 'markdown-insert-header-atx-1)
  (define-key markdown-mode-map (kbd "C-c 2") 'markdown-insert-header-atx-2)
  (define-key markdown-mode-map (kbd "C-c 3") 'markdown-insert-header-atx-3)
  (define-key markdown-mode-map (kbd "C-c 4") 'markdown-insert-header-atx-4)
  (define-key markdown-mode-map (kbd "C-c 5") 'markdown-insert-header-atx-5)
  (define-key markdown-mode-map (kbd "C-c 6") 'markdown-insert-header-atx-6)
  (set-fill-column 80)
  (turn-on-auto-fill)
  (flyspell-mode))

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-css-property
		    ac-source-abbrev
		    ac-source-dictionary
		    ac-source-words-in-same-mode-buffers))))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (add-hook 'web-mode-hook
	    (lambda ()
	      (setq web-mode-style-padding 2)
	      (emmet-mode)
	      (flycheck-add-mode 'html-tidy 'web-mode)
	      (flycheck-mode))))

(use-package mmm-mode
  :ensure t)

(use-package python-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook
	    (lambda ()
	      (add-to-list 'write-file-functions 'delete-trailing-whitespace))))


;; RUST
(use-package rust-mode
  :ensure t)

(use-package flycheck-rust
  :ensure t
  :defer t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package flycheck-package
  :ensure t
  :init (with-eval-after-load 'flycheck (flycheck-package-setup)))

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package rustfmt
  :ensure t
  :config
  (define-key rust-mode-map (kbd "C-c C-f") #'rustfmt-format-buffer))

(use-package tide
  :ensure t)

;;;;;;;;;;;;;;;;;;;
;; Other Modules ;;
;;;;;;;;;;;;;;;;;;;

(use-package writeroom-mode
  :ensure t)

;;;;;;;;;;;;;;;;;
;; Other Hooks ;;
;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (eldoc-mode)
	    (highlight-symbol-mode)
	    (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))

(add-hook 'javascript-mode-hook
	  (lambda ()
	    (set-fill-column 120)
	    (turn-on-auto-fill)
	    (setq js-indent-level 2)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(package-selected-packages
   (quote
    (wc-mode tide hexo elfeed fancy-battery spaceline rustfmt json-mode python-mode web-mode flycheck-clojure clojure-mode helm-flx company-flx flx helm-projectile evil-smartparens smartparens helm-smex smex marmalade evil-magit magit wgrep-helm swiper ag exec-path-from-shell company helm-gtags evil-org highlight-symbol flycheck projectile evil-visual-mark-mode powerline-evil evil helm)))
 '(tab-stop-list
   (quote
    (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'init-linum)
(load-theme 'monokai t)

(provide 'init)
;;; init ends here
