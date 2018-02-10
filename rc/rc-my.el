;;; rc-my.el ---                                     -*- lexical-binding: t; -*-
;;; Commentary:

;; This file contains all config to initialize the Emacs packages
;; written by me.
;; 這檔案放的是我寫的 Emacs 外掛所使用的設定檔。

;;; Code:

;; ======================================================
;; cakecrumbs.el
;; ======================================================
(require 'cakecrumbs)
(setq cakecrumbs-refresh-delay-seconds 0)
(cakecrumbs-auto-setup)

;; ======================================================
;; resize-fram.el
;; ======================================================

(require 'resize-frame)
(global-set-key (kbd "M-`") 'resize-frame)

;;======================================================
;; kaomoji.el
;;======================================================
(require 'kaomoji)

;;======================================================
;; tldr.el
;;======================================================
(require 'tldr)

;;======================================================
;; fsc.el Fuck the Speeching Censorship
;;======================================================

(require 'fsc)
(global-set-key (kbd "C-c f s") 'fsc)


;; ======================================================
;; hexo.el
;; ======================================================
(require 'hexo)

;;======================================================
;; writing-utils.el 寫作加強
;;======================================================
;; Details: https://github.com/kuanyui/writing-utils.el
(require 'writing-utils)
(require 'page-title)
(require 'flickr)
(require 'markdown-and-html)
(require 'markdown-toc)
(require 'minibuffer-enhancements)
(global-set-key (kbd "C-a") #'beginning-of-line-or-indentation)

;; [自用] 把livedoor Reader輸出的opml檔轉成markdown，然後吐到hexo目錄。
(require 'livedoor-opml-to-markdown)

;;======================================================
;; moe-theme.el
;;======================================================

(add-to-list 'custom-theme-load-path "~/.emacs.d/git/moe-theme/")

(require 'moe-theme)
(require 'moe-theme-flavours)

(setq moe-theme-colorize-modeline-by-frame-id t)
;; (setq moe-theme-resize-title-markdown '(1.5 1.4 1.3 1.2 1.0 1.0))
;; (setq moe-theme-resize-title-org '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
;; (setq moe-theme-resize-title-rst '(1.5 1.4 1.3 1.2 1.1 1.0))
(load-theme 'moe-dark t nil)
;; (moe-theme-apply-color 'cyan)
;; (moe-dark)

;;(moe-theme-random-color)
;;(powerline-moe-theme)

(global-set-key (kbd "C-c m d") 'moe-dark)
(global-set-key (kbd "C-c m l") 'moe-light)


;;======================================================
;; moedict.el 萌典
;;======================================================
(require 'moedict)
;;(require 'moedict-stroke)
(global-set-key (kbd "C-c m m") 'moedict)
;; ======================================================
;; fm-bookmarks.el
;; ======================================================
(require 'fm-bookmarks)

;; Available options: kde4, gnome3, pcmanfm, custom
;; Multiple file managers are acceptable.
;; Notice that 'media currently is only available on Unix-like OS
(setq fm-bookmarks-enabled-file-managers '(kde5 custom media))

;; Add customized bookmarks
(setq fm-bookmarks-custom-bookmarks
      '(("Root" . "/")
        ("Tmp" . "/tmp/")
        ))

;; Hide item by name/path pattern
(setq fm-bookmarks-hide-by-name-pattern '("Bluetooth" "Images"))
(setq fm-bookmarks-hide-by-path-pattern '())

;; Don't show duplicated item. (Because the same path may be added by
;; different FMs)
(defvar fm-bookmarks-hide-duplicated t)

;; "Mounted media" is an experimental function. If you don't want
;; this, set this to nil.
(setq fm-bookmarks-enable-mounted-media t)

;; Use cache to avoid regenerating list every time.
(setq fm-bookmarks-enable-cache t)

;; Shortcut to open FM bookmark.
(global-set-key (kbd "C-x `") #'fm-bookmarks)
;; Use ` to open FM bookmark in Dired-mode
(define-key dired-mode-map (kbd "`") #'fm-bookmarks)

;; ======================================================
;; ta.el
;; ======================================================
(require 'ta)

(setq ta-delay 0.1)
(setq ta-homophony-list
      '(("他" "她" "它" "牠" "祂")
        ("你" "妳")
        ("的" "得")
        ("訂" "定")
        ("作" "做" "坐")
        ("在" "再")
        ("板" "版")
        ))

(mapc (lambda (mode-hook) (add-hook mode-hook 'ta-mode))
      '(org-mode-hook
        markdown-mode-hook
        rst-mode-hook
        twittering-edit-mode-hook))

;; Modify the character under cursor
;; 修改游標下的字
(define-key ta-mode-map (kbd "M-p") 'ta-previous-homophony)
(define-key ta-mode-map (kbd "M-n") 'ta-next-homophony)
;; Move cursor left/right to possible character
;; 把目前游標向左/右移動至可能的錯字
(define-key ta-mode-map (kbd "M-i") 'ta-left)
(define-key ta-mode-map (kbd "M-o") 'ta-right)

;; ======================================================
;; taiwan-holidays.el 台灣的節慶支援
;; ======================================================
(require 'taiwan-holidays)
(setq mark-holidays-in-calendar t)
(setq calendar-mark-holidays-flag t)
(setq taiwan-holidays-important-holidays taiwan-holidays-taiwan-holidays)
(setq calendar-holidays taiwan-holidays-important-holidays)


(provide 'rc-my)
;;; rc-my.el ends here
