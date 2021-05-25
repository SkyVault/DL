;;; Package --- Dustin H. Neumanns Emacs config
;;; Commentary:

(require 'paren)

;; Package loading / config

;;; Controls / GUI and Themes

(use-package evil
  :ensure t
  :init ; Set variables here
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-default-state 'normal)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil magit
  :ensure t
  :config
  (evil-collection-init))

(use-package sexy-monochrome-theme
  :ensure t
  :config
  (load-theme 'sexy-monochrome t))

(use-package nav-flash
  :ensure t
  :config
  (nav-flash-show))

(use-package helm
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(use-package helm-ag
  :after helm
  :ensure t
  :config
  (global-set-key (kbd "C-x /") 'helm-do-ag-project-root))

(use-package rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;; Emacs project management and Git

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1)
  (global-set-key (kbd "C-x p p") 'projectile-switch-project)
  (global-set-key (kbd "C-x p k") 'projectile-kill-buffers))

(use-package magit :ensure t)

;;; LSP Config

(use-package web-mode
  :ensure t
  :mode (("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.jsx\\'" . web-mode)
	 ("\\.js\\'" . web-mode)
	 ("\\.json\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :commands web-mode
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package lsp-mode
  :after web-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook #'lsp))

;; GUI Config

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(show-paren-mode 1)
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)

(setq make-backup-files nil)

;;; Start screen

(setq inhibit-startup-message t)

(provide 'config)
;;; Config ends here
