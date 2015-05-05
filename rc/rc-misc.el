;;; rc-misc.el ---                                   -*- lexical-binding: t; -*-

;; misc 雜項

;;======================================================
;; fsc.el Fuck the Speeching Censorship
;;======================================================
(add-to-list 'load-path "~/.emacs.d/under-construction/fsc/")
(load-file "~/.emacs.d/under-construction/fsc/fsc.el")

(require 'fsc)
(global-set-key (kbd "C-c f s") 'fsc)

;;======================================================
;; writing-utils.el 寫作加強
;;======================================================
;; Details: https://github.com/kuanyui/writing-utils.el
(add-to-list 'load-path "~/.emacs.d/git/writing-utils/")
(require 'writing-utils)
(require 'page-title)
(require 'hexo)
(require 'flickr)
(require 'markdown-and-html)

;; [自用] 把livedoor Reader輸出的opml檔轉成markdown，然後吐到hexo目錄。
(require 'livedoor-opml-to-markdown)

;;======================================================
;; moe-theme.el
;;======================================================

(add-to-list 'load-path "~/.emacs.d/lisps/powerline/")
(require 'powerline)

(add-to-list 'custom-theme-load-path "~/.emacs.d/git/moe-theme/")
(add-to-list 'load-path "~/.emacs.d/git/moe-theme/")

(require 'moe-theme)
(setq moe-theme-highlight-buffer-id nil)
(setq moe-theme-resize-markdown-title '(1.5 1.4 1.3 1.2 1.0 1.0))
(setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
(setq moe-theme-resize-rst-title '(1.5 1.4 1.3 1.2 1.1 1.0))

(moe-theme-set-color 'cyan)
(moe-light)

;;(moe-theme-random-color)
;;(powerline-moe-theme)

(global-set-key (kbd "C-c m d") 'moe-dark)
(global-set-key (kbd "C-c m l") 'moe-light)


;;======================================================
;; moedict.el 萌典
;;======================================================
(add-to-list 'load-path "~/.emacs.d/git/moedict/")
(require 'moedict)
(global-set-key (kbd "C-c d m") 'moedict-lookup)


;; ======================================================
;; fm-bookmarks.el
;; ======================================================
(add-to-list 'load-path "~/.emacs.d/git/fm-bookmark/")
(require 'fm-bookmark)

;; Available options: kde4, gnome3, pcmanfm, custom
;; Multiple file managers are acceptable.
;; Notice that 'media currently is only available on Unix-like OS
(setq fm-bookmark-enabled-file-managers '(kde4 gnome3 pcmanfm custom media))

;; Add customized bookmarks
(setq fm-bookmark-custom-bookmarks
      '(("Root" . "/")
	("Tmp" . "/tmp/")
	))

(setq fm-bookmark-enable-mounted-media t)

(global-set-key (kbd "C-x `") #'fm-bookmark)
;; Use ` to open in Dired-mode
(define-key dired-mode-map (kbd "`") #'fm-bookmark)

;; ======================================================
;; ta.el
;; ======================================================
(add-to-list 'load-path "~/.emacs.d/git/ta.el/")
(require 'ta)

(mapc (lambda (mode-hook) (add-hook mode-hook 'ta-mode))
      '(org-mode-hook
        markdown-mode-hook
        rst-mode-hook
        twittering-edit-mode-hook))
;;======================================================
;; Wikipedia
;;======================================================

(require 'wikipedia-mode)
(add-to-list 'auto-mode-alist '("\\.wiki\\'" . wikipedia-mode))

;;======================================================
;; Google Translate
;;======================================================

(require 'text-translator)
(require 'text-translator-load)
(global-set-key "\C-x\M-t" 'text-translator)
(global-set-key "\C-x\M-T" 'text-translator-translate-last-string)
(setq text-translator-auto-selection-func 'text-translator-translate-by-auto-selection-entw)
(global-set-key "\C-xt" 'text-translator-translate-by-auto-selection)

(setq google-translate-default-source-language "en"
      google-translate-default-target-language "zh")


(provide 'rc-misc)
;;; rc-misc.el ends here
