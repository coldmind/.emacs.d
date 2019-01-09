;;; init.el --- coldmind's Emacs configuration

;;; Commentary:

;;; Code:

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; PATH
(let ((path (shell-command-to-string ". ~/.bash_profile; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; Some term enhancement
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defadvice ansi-term (before force-bash)
  (interactive (list "/bin/zsh")))
(ad-activate 'ansi-term)

;; Other configs
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Splash Screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode  1)

;; Paragraph movement
(global-set-key (kbd "s-j") 'forward-paragraph)
(global-set-key (kbd "s-k") 'backward-paragraph)

;; Keybinding for term mode
(add-hook 'term-mode
          (lambda () (global-set-key (kbd "s-v") 'term-paste)))

;; OrgMode Configs
(setq org-html-validation-link nil)
(setq org-todo-keywords
      '((sequence "TODO" "WORKING" "HOLD" "|" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO"    . "blue")
	("WORKING" . "yellow")
	("HOLD"    . "red")
	("DONE"    . "green")))

;; UI configurations
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))
(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-linum-mode 1)
(add-to-list 'default-frame-alist '(font . "Iosevka-11"))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

;; Anzu for search matching
(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

;; Theme
(use-package goose-theme
  :ensure t
  :config
  (load-theme 'goose t))

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-M-x-fuzzy-match t
	helm-mode-fuzzy-match t
	helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t
	helm-locate-fuzzy-match t
	helm-semantic-fuzzy-match t
	helm-imenu-fuzzy-match t
	helm-completion-in-region-fuzzy-match t
	helm-candidate-number-list 80
	helm-split-window-in-side-p t
	helm-move-to-line-cycle-in-source t
	helm-echo-input-in-header-line t
	helm-autoresize-max-height 0
	helm-autoresize-min-height 20)
  :config
  (helm-mode 1))

;; RipGrep
(use-package helm-rg :ensure t)

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode 1))

;; Helm Projectile
(use-package helm-projectile
  :ensure t
  :init
  (setq helm-projectile-fuzzy-match t)
  :config
  (helm-projectile-on))

;; NeoTree
(use-package neotree
  :ensure t)

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)
 
;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; LSP
(use-package lsp-mode
  :ensure t
  :init
  (add-hook 'prog-major-mode #'lsp-prog-major-mode-enable))

(use-package lsp-ui
  :ensure t
  :init
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Company mode
(use-package company
  :ensure t
  :init
  (setq company-minimum-prefix-length 3)
  (setq company-auto-complete nil)
  (setq company-idle-delay 0)
  (setq company-require-match 'never)
  (setq company-frontends
	'(company-pseudo-tooltip-unless-just-one-frontend
	  company-preview-frontend
	  company-echo-metadata-frontend))
  (setq tab-always-indent 'complete)
  (defvar completion-at-point-functions-saved nil)
  :config
  (global-company-mode 1)
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-for-tab-command)
  (defun company-indent-for-tab-command (&optional arg)
    (interactive "P")
    (let ((completion-at-point-functions-saved completion-at-point-functions)
    	  (completion-at-point-functions '(company-complete-common-wrapper)))
      (indent-for-tab-command arg)))

(defun company-complete-common-wrapper ()
	(let ((completion-at-point-functions completion-at-point-functions-saved))
	(company-complete-common))))

(use-package company-lsp
  :ensure t
  :init
  (push 'company-lsp company-backends))
 
;; Powerline
(use-package spaceline
  :ensure t
  :init
  (setq powerline-default-separator 'slant)
  :config
  (spaceline-emacs-theme)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-buffer-size-off))

;; Undo-tree

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode))

;; Wind Move

(use-package windmove
  :ensure t
  :init (windmove-default-keybindings))

;; Git
(use-package magit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;
;; Language Supports ;;
;;;;;;;;;;;;;;;;;;;;;;;
 
;; Rust

(use-package rust-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package flycheck-rust
  :ensure t)

(use-package cargo
  :ensure t)

(use-package racer
  :ensure t
  :init
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

(with-eval-after-load 'rust-mode
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
  (add-hook 'rust-mode-hook
          '(lambda ()
             (local-set-key (kbd "TAB") #'company-indent-or-complete-common)
	     (electric-pair-mode 1))))

;; Python

(use-package anaconda-mode
  :ensure t
  :init
  (add-hook 'python-mode-hook #'anaconda-mode)
  (add-hook 'python-mode-hook #'anaconda-eldoc-mode))

(with-eval-after-load 'anaconda-mode
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "--simple-prompt -i"))

;;;;;;;;;;;;;;;;;;;;;;;
;;        Etc        ;;
;;;;;;;;;;;;;;;;;;;;;;;

(provide 'init)
;;; init.el ends here

;; Auto-generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (racer which-key use-package spaceline rustic neotree magit lsp-ui helm-rg helm-projectile goose-theme general flymake-rust flycheck-rust eglot doom-themes company-lsp cargo anzu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
