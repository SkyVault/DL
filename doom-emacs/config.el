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

(setq doom-theme 'doom-moonlight)

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

(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode)

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

;; (parrot-set-parrot-type 'default)
(add-hook 'after-save-hook 'parrot-start-animation)
