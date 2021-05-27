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

;; Org config

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-hide-empasis-markers t)

(font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
	(0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(let* ((variable-tuple
	(cond
	 ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
	 ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
	 ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
	 ((x-list-fonts "Verdana")         '(:font "Verdana"))
	 ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
	 (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
	(base-font-color     (face-foreground 'default nil 'default))
	(headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces
    'user
    `(org-level-8 ((t (,@headline ,@variable-tuple))))
    `(org-level-7 ((t (,@headline ,@variable-tuple))))
    `(org-level-6 ((t (,@headline ,@variable-tuple))))
    `(org-level-5 ((t (,@headline ,@variable-tuple))))
    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.0))))
    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.2 :weight normal))))
    `(org-level-1 ((t (,@headline ,@variable-tuple :height 2.00))))
    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

(custom-theme-set-faces
'user
'(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin))))
'(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;;; Start screen

(setq inhibit-startup-message t)

(provide 'config)
;;; Config ends here
