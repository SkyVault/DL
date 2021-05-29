(require 'package)

;; Initialize melpa archives

(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'load-path "~/.config/emacs/config")

(require 'config)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(slime-company treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs slime org-bullets helm-ag web-mode lsp-mode company flycheck evil-collection evil-magit magit projectile rainbow-delimiters helm nav-flash which-key sexy-monochrome-theme evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Fira Code Retina" :height 160))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "ETBembo" :height 2.0 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "ETBembo" :height 2.0))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "ETBembo" :height 1.2 :weight normal))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "ETBembo" :height 1.1))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "ETBembo" :height 1.0))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "ETBembo"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "ETBembo"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "ETBembo"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#d3d3d3" :font "ETBembo"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "ETBembo" :height 180 :weight thin)))))
