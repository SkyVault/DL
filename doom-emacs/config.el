;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(doom/set-indent-width 2)
(doom/set-frame-opacity 0.9)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Dustin Neumann"
      user-mail-address "dustinneumann42@gmail.com")

(setq doom-font (font-spec :family "Fira Code" :size 18))

(setq doom-theme 'sexy-monochrome)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq confirm-kill-emacs nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(setq sgml-quick-keys 'close)

(setq company-dabbrev-downcase 0)
(setq company-idle-delay 0)

;; Nice for org mode having mixed fonts
;; (add-hook! 'org-mode-hook #'mixed-pitch-mode)
;; (setq mixed-pitch-variable-pitch-cursor nil)

;; Org mode config
(after! org
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t))

(setq org-hide-emphasis-markers t)

(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode
           #'org-bullets-mode)

(let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (headline `(:weight bold)))

  (custom-theme-set-faces
    'user
    `(org-level-8 ((t (,@headline ,@variable-tuple))))
    `(org-level-7 ((t (,@headline ,@variable-tuple))))
    `(org-level-6 ((t (,@headline ,@variable-tuple))))
    `(org-level-5 ((t (,@headline ,@variable-tuple))))
    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
    `(org-level-1 ((t (,@headline ,@variable-tuple :height 2.00))))
    `(org-document-title ((t (,@headline ,@variable-tuple :height 3.0 :underline nil)))))

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
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

;; Rss feeds
(setq elfeed-feeds
  '("http://lukesmith.xyz/rss.xml"
    "https://notrelated.libsyn.com/rss"
    "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA"
    "https://www.archlinux.org/feeds/news/"
    "https://feeds.simplecast.com/C6NQglnL"
    "https://zserge.com/rss.xml"
    "https://feeds.transistor.fm/thoughts-on-functional-programming-podcast-by-eric-normand"
    "https://coder.show/rss"
    "https://linuxunplugged.com/rss"
    "https://defungames.com/feed/"
    "http://feeds.feedburner.com/RoguelikeRadio"
    "http://mediapub.it.ox.ac.uk/feeds/137514/audio.xml"
    "http://feeds.feedburner.com/NoDogmaPodcast"
    "https://lexfridman.com/feed/podcast/"
    "https://feeds.simplecast.com/L9810DOa"))

;; (defun ga/play-with-mpv (start end)
;;   "Play the link in the region with mpv"
;;   (interactive "r")
;;   (shell-command (concat "mpv " (buffer-substring start end) "\&")))

;; (define-key elfeed-show-mode-map (kbd "C-c o") 'ga/play-with-mpv)

(defun arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
          (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
  (delete-region start end)
  (insert insertion)))

(custom-set-variables
 '(zoom-mode t))

(custom-set-variables
 '(zoom-size '(0.618 . 0.618)))

;; shellhaters (great unix reference)
(defun shellhaters ()
  (interactive
   (eww-browse-url "shellhaters.org")))

(defun web ()
  (interactive)
  (let ((url (read-string "URL:")))
        (eww-browse-url url)))

(defun ddg ()
  (interactive)
  (eww-browse-url "duckduckgo.com"))

(defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

;; Haxe
(add-to-list 'auto-mode-alist '("\\.hx\\'" . haxe-mode))
