;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;(package! zoom)
;;(package! org-bullets)
;;(package! sexy-monochrome-theme)
;;(package! fennel-mode)
;; (package! haxe-mode)
;; (package! battle-haxe)

;; This isn't really a package, it just provides a `haxe-mode' to work with
;; (use-package haxe-mode
;;   :mode ("\\.hx\\'" . haxe-mode)
;;   :no-require t
;;   :init
;;   (require 'js)
;;   (define-derived-mode haxe-mode js-mode "Haxe"
;;     "Haxe syntax highlighting mode. This is simply using js-mode for now."))

;; (use-package battle-haxe
;;   :hook (haxe-mode . battle-haxe-mode)
;;   :bind (("S-<f4>" . #'pop-global-mark) ;To get back after visiting a definition
;;          :map battle-haxe-mode-map
;;          ("<f4>" . #'battle-haxe-goto-definition)
;;          ("<f12>" . #'battle-haxe-helm-find-references))
;;   :custom
;;   (battle-haxe-yasnippet-completion-expansion t "Keep this if you want yasnippet to expand completions when it's available.")
;;   (battle-haxe-immediate-completion nil "Toggle this if you want to immediately trigger completion when typing '.' and other relevant prefixes."))
