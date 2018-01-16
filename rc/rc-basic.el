;;; basic.el ---                                     -*- lexical-binding: t; -*-

;;kuanyui's ~/.emacs

;;(setq user-mail-address "azazabc123@gmail.com")
;;(setq user-full-name "kuanyui")

(set-locale-environment "UTF-8")

(defmacro buffer-real-name ()
  "This macro will return the real filename of current
buffer (without parent directory) Because `uniquify' could cause
`buffer-name' returning you an unwanted value.
e.g. ruby main.rb => ruby main.rb:directory_name"
  `(file-name-nondirectory buffer-file-name))

;; packages which is not installed via packages.el
(add-to-list 'load-path "~/.emacs.d/lisps")

(mapc (lambda (x) (add-to-list 'load-path x))
      (directory-files "~/.emacs.d/git" t "^[^.]"))

;; Packges.el
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)


(require 'subr-x)
(cond ((member system-type '(darwin gnu/linux))
       (setq shell-file-name "/bin/zsh")
       (setq shell-command-switch "-ic")
       (setenv "PATH" (string-join (list (getenv "PATH")
                                         "/usr/local/bin/"
                                         (concat (getenv "HOME") "/.cabal/bin/")
                                         (concat (getenv "HOME")"/.scripts/"))
                                   ":")))
      ((member system-type 'cygwin) (setq shell-file-name "/bin/bash")))

;;(require 'whitespace)
;; (global-whitespace-mode)
;; (whitespace-mode)



;; (let ((mozc-path "/usr/share/emacs/site-lisp/mozc.el"))
;;    (when (file-exists-p mozc-path)
;;      (load-file mozc-path)
;;      (setq default-input-method "japanese-mozc")
;;      ;; ac-mozc
;;      (load-file "~/.emacs.d/lisps/ac-mozc/ac-mozc.el")
;;      (define-key ac-mode-map (kbd "C-c C-\\") 'ac-complete-mozc)
;;
;;      (require 'org)
;;      (add-to-list 'ac-modes 'org-mode)
;;
;;      (defun my-ac-mozc-setup ()
;;        (setq ac-sources
;;              '(ac-source-mozc ac-source-ascii-words-in-same-mode-buffers))
;;        (set (make-local-variable 'ac-auto-show-menu) 0.2))
;;      (add-hook 'org-mode-hook 'my-ac-mozc-setup)))

;;-==================================================
;; GUI Emacs
;;======================================================

;;GUI Emacs調整字體大小
;;(when (window-system)
;;(defun sacha/increase-font-size ()
;;  (interactive)
;;  (set-face-attribute 'default
;;                      nil
;;                      :height
;;                      (ceiling (* 1.10
;;                                  (face-attribute 'default :height))))
;;  (apply-font-setting))
;;
;;(defun sacha/decrease-font-size ()
;;  (interactive)
;;  (set-face-attribute 'default
;;                      nil
;;                      :height
;;                      (floor (* 0.9
;;                                (face-attribute 'default :height))))
;;  (apply-font-setting))
;;
;;;;　GUI版本下的中文字體問題
;;(defun apply-font-setting ()
;;  (interactive)
;;  (if (window-system)
;;      (progn
;;        (dolist (charset '(han kana symbol cjk-misc bopomofo))
;;          (set-fontset-font (frame-parameter nil 'font)
;;                            charset
;;                            (font-spec :family "文泉驛等寬微米黑"))))))
;;
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;         (set-fontset-font (frame-parameter nil 'font) charset
;;                           (font-spec :family "文泉驛等寬微米黑" :spacing 100 :size nil)))
;; (if (window-system)
;;     (progn
;;       (set-face-attribute 'default nil :height 90)
;;       (apply-font-setting)))
;; (global-set-key (kbd "C-+") 'sacha/increase-font-size)
;;(global-set-key (kbd "C--") 'sacha/decrease-font-size)
;;)
(global-set-key (kbd "C-c C-r") 'revert-buffer-without-confirm)
(defun revert-buffer-without-confirm ()
  (interactive)
  (revert-buffer nil t)
  (message "Buffer reverted."))

;; ======================================================
;; Make window status undo-able
;; ======================================================
(require 'winner)
(winner-mode 1)

;; ======================================================
;; Find file at point
;; ======================================================
(require 'ffap)
(global-set-key (kbd "C-x C-f") (lambda () (interactive)
                                  (find-file-at-point)
                                  (move-end-of-line 1))) ;Why not work?!

;; shit not work...
(defadvice find-file-at-point (after auto-goto-eol activate)
  (move-end-of-line 1))

(defadvice ffap-file-finder (after auto-goto-eol activate)
  (move-end-of-line 1))



;; ============================================
;; Coldnew's Font Size Conf for Org-Table
;; ============================================
;; 特殊字型設定
(when (window-system)
  (if (eq system-type 'windows-nt)
      (set-face-attribute 'default nil :font "Consolas-9"))

  (defvar emacs-english-font "DejaVu Sans Mono" "The font name of English.")
  (defvar emacs-cjk-font "文泉驛等寬微米黑" "The font name for CJK.")
  (find-font (font-spec :name "LiHei Pro"))
  (font-family-list)
  (if (eq system-type 'windows-nt)
      (setq emacs-cjk-font "Consolas"
            emacs-english-font "Consolas")
    (setq emacs-cjk-font "WenQuanYi Micro Hei Mono"))

  (defvar emacs-font-size-pair '(12 . 14)
    "Default font size pair for (english . chinese)")

  (defvar emacs-font-size-pair-list
    '(( 5 .  6) (9 . 10) (10 . 12)(12 . 14)
      (13 . 16) (15 . 18) (17 . 20) (19 . 22)
      (20 . 24) (21 . 26) (24 . 28) (26 . 32)
      (28 . 34) (30 . 36) (34 . 40) (36 . 44))
    "This list is used to store matching (englis . chinese) font-size.")

  (defun font-exist-p (fontname)
    "Test if this font is exist or not."
    (if (or (not fontname) (string= fontname ""))
        nil
      (if (not (x-list-fonts fontname)) nil t)))

  (defun set-font (english chinese size-pair)
    "Setup emacs English and Chinese font on x window-system."

    (if (font-exist-p english)
        (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

    (if (font-exist-p chinese)
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font) charset
                            (font-spec :family chinese :size (cdr size-pair))))))
  ;; Setup font size based on emacs-font-size-pair
  (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)

  (defun emacs-step-font-size (step)
    "Increase/Decrease emacs's font size."
    (let ((scale-steps emacs-font-size-pair-list))
      (if (< step 0) (setq scale-steps (reverse scale-steps)))
      (setq emacs-font-size-pair
            (or (cadr (member emacs-font-size-pair scale-steps))
                emacs-font-size-pair))
      (when emacs-font-size-pair
        (message "emacs font size set to %.1f" (car emacs-font-size-pair))
        (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

  (defun increase-emacs-font-size ()
    "Decrease emacs's font-size acording emacs-font-size-pair-list."
    (interactive) (emacs-step-font-size 1))

  (defun decrease-emacs-font-size ()
    "Increase emacs's font-size acording emacs-font-size-pair-list."
    (interactive) (emacs-step-font-size -1))

  (global-set-key (kbd "C-=") 'increase-emacs-font-size)
  (global-set-key (kbd "C--") 'decrease-emacs-font-size)
  )
;;======================================================
;; 基本設定
;;======================================================
;; C-x C-f 不區分大小寫
(setq read-file-name-completion-ignore-case t)

(setq require-final-newline t)
;;超變態的undo-tree-mode
;;(提醒：redo會變成C-?)
;;C-x u 進入 undo-tree-visualizer-mode，t顯示時間戳。
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C-M-_") 'undo-tree-redo)

;;recents最近開啟的檔案，C-x C-r
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 35)
;;(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x C-r") 'helm-recentf)

;;(run-with-timer 0 (* 10 60) 'recentf-save-list)
(setq calc-group-char " ")
(setq calc-group-digits 4)

;; Always follow symbol link to real file when find-file.
(setq vc-follow-symlinks t)

;; 讓手遠離方向鍵
(global-set-key (kbd "M-I") 'previous-line)
(global-set-key (kbd "M-J") 'left-char)
(global-set-key (kbd "M-K") 'next-line)
(global-set-key (kbd "M-L") 'right-char)
(global-set-key (kbd "M-N") 'next-buffer)
(global-set-key (kbd "M-P") 'previous-buffer)

(global-set-key (kbd "M-S") 'windmove-up)
(global-set-key (kbd "M-X") 'windmove-down)
(global-set-key (kbd "M-C") 'windmove-right)
(global-set-key (kbd "M-Z") 'windmove-left)

;; Show line-number in the mode line
(line-number-mode 1)
;; Show column-number in the mode line
(column-number-mode t)

;; 行號
;; 設定例外：
;; 1. 某些mode開linum-mode會錯亂，例如term。遇到這類mode就不開啟linum。
;; 2. 當Org檔案太大太長太多標題時，行數會使Emacs變得非常遲緩，所以這裡設定成在開檔案時，
;; 如果為org檔案且行數 >1000 就不開linum-mode。
(setq linum-right-space nil)
(setq linum-left-space nil)
(setq linum-format 'dynamic)

;; 預設就把萬惡的linum關掉 [2016-03-31 木 19:10]
;;(global-linum-mode t)


(setq inhibit-linum-mode-alist
      `(eshell-mode
        shell-mode
        term-mode
        erc-mode
        compilation-mode
        woman-mode
        w3m-mode
        magit-mode
        magit-status-mode
        ,(if (not (window-system)) 'twittering-mode)
        ))

(defadvice linum-on (around inhibit-for-modes activate)
  "Stop turing linum-mode if it is in the inhibit-linum-mode-alist."
  (unless (or (member major-mode inhibit-linum-mode-alist)
              (and (eq major-mode 'org-mode)
                   (> (count-lines (point-min) (point-max)) 1000)))
    ad-do-it))                          ;WTF


(defun my-find-alternate-file ()
  "Don't remain file name in minibuffer"
  (interactive)
  (find-alternate-file (read-file-name "Find alternate file: ")))
(global-set-key (kbd "C-x C-v") 'my-find-alternate-file)


;; Indicate buffer size in mode-line
(setq size-indication-mode t)

;; Highlight line number
(require 'hlinum)
(hlinum-activate)


;;當前行高亮顯示
;; (global-hl-line-mode 1)

;;emacs內建書籤存檔
(setq bookmark-save-flag 1)

;;在標題顯示文件名稱(%b)與路徑(%f)
(setq frame-title-format "%n%b (%f) - %F")
;;(setq frame-title-format '((:eval default-directory)))

;;把捲軸移到右側
(customize-set-variable 'scroll-bar-mode 'right)

(require 'hungry-delete)
(global-hungry-delete-mode t)
(global-set-key (kbd "C-x <deletechar>") 'global-hungry-delete-mode)
;; (global-set-key (kbd "C-c <deletechar>") 'global-hungry-delete-mode)

;; 幹掉沒啥屁用只會按錯的<menu>
(global-unset-key (kbd "<menu>"))

;; 讓(previous-buffer)和(next-buffer)自動跳過沒啥用的buffer例如*Message*。
;; *Help*和*scratch*是例外。

;;(defun buffer-all-special-p ()
;;  "If all buffers are \"special buffers\"(buffer whose name
;; is '^\*.+\*$'. e.g. *scratch* ), return non-nil.
;;If there is any normal buffer, return nil."
;;  (not
;;   (some #'identity
;;         ;;if all nil: all buffer are "^*.+*$"
;;         (mapcar (lambda (x) (string-match "^\\*.+\\*$" x))
;;                 (mapcar #'buffer-name (buffer-list))))))
;;
;;(defadvice next-buffer (after auto-skip-next-buffer activate)
;;  "讓`next-buffer'自動跳過沒啥用的buffer，例如*Message*。
;;*Help*和*scratch*是例外。"
;;  ;; [FIXME] 當所有buffer全是*NAME*時，可能造成無限迴圈。
;;  (when (not (buffer-all-special-p)) ;any buffer is NOT special buffers.
;;    (if (not (string-match "^\\*\\(?:Help\\|scratch\\)\\*$"
;;                           (buffer-name)))
;;        (when (string-match "^\\*.+\\*$" (buffer-name))
;;          (next-buffer)))))
;;
;;(defadvice previous-buffer (after auto-skip-previous-buffer activate)
;;  "讓`previous-buffer'自動跳過沒啥用的buffer，例如*Message*。
;;*Help*和*scratch*是例外。"
;;  ;; [FIXME] 當所有buffer全是*NAME*時，可能造成無限迴圈。
;;  (when (not (buffer-all-special-p)) ;any buffer is NOT special buffers.
;;    (if (not (string-match "^\\*\\(?:Help\\|scratch\\)\\*$"
;;                           (buffer-name)))
;;        (when (string-match "^\\*.+\\*$" (buffer-name))
;;          (next-buffer)))))

;;讓Isearch不會再主動清除搜尋的高亮顯示
(setq lazy-highlight-cleanup nil)

;; 測試 universal-argument prefix的值用。
(defun display-prefix (arg)
  "Display the value of the raw prefix arg."
  (interactive "P")
  (message "%s" arg))

;;凸顯括號位置（而不是來回彈跳）
(show-paren-mode t)
;;(setq show-paren-style 'parentheses)
(setq show-paren-style 'expression) ;;另一種突顯方式(突顯整個括號範圍)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;;X Clipboard在游標處插入，而不是滑鼠點擊的地方插入。
(setq mouse-yank-at-point t)

;;讓Emacs可以直接打開/顯示圖片。
(setq auto-image-file-mode t)



;;同名檔案不混淆（同名檔案同時開啟時，會在buffer加上目錄名稱）
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'forward
 uniquify-separator "/")

;;換掉歡迎畫面的難看GNU Logo
;;(setq  fancy-splash-image "~/.emacs.d/icon.png")
;;完全隱藏歡迎畫面
(setq inhibit-splash-screen t)

(setq initial-scratch-message
      ";; If you have no shadows, you're not in the light
;; -- Lady Gaga
")

;; aspell
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-dictionary "american")


;;靠近螢幕邊緣三行時就開始捲動，比較容易看上下文
(setq scroll-margin 3)

;;關閉煩人的錯誤提示音，改為在螢幕上提醒。
(setq visible-bell t)

;;超大kill-ring. 防止不小心删掉重要的東西。
(setq kill-ring-max 200)

;;Tab改為插入空格
;;abs look fine on a terminal or with ordinary printing, but they produce badly indented output when you use TeX or Texinfo since TeX ignores tabs.
;; (setq standard-indent 4)

;;每次修改文件自動更新Time-stamp
;;将time-stamp加入write-file-hooks，就能在每次保存文件时自动更新time-stamp
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-format
      "此文件最後是在%04y-%02m-%02d %02H:%02M:%02S由%:u修改"
      time-stamp-active t
      time-stamp-warn-inactive t)


;;Emacs內建的自動補完hippie-expand
(global-set-key [(meta ?/)] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev                 ; 搜索当前 buffer
        try-expand-dabbrev-visible         ; 搜索当前可见窗口
        try-expand-dabbrev-all-buffers     ; 搜索所有 buffer
        try-expand-dabbrev-from-kill       ; 从 kill-ring 中搜索
        try-complete-file-name-partially   ; 文件名部分匹配
        try-complete-file-name             ; 文件名匹配
        try-expand-all-abbrevs             ; 匹配所有缩写词
        try-expand-list                    ; 补全一个列表
        try-expand-line                    ; 补全当前行
        try-complete-lisp-symbol-partially ; 部分补全 elisp symbol
        try-complete-lisp-symbol))         ; 补全 lisp symbol

(load "~/.emacs.d/lisps/complete-with-calc.el")

;;補全另一選擇company-mode
;;(add-to-list 'load-path "~/.emacs.d/lisps/company")
;;(autoload 'company-mode "company" nil t)
;;(company-mode 1)

;;popup-kill-ring
(add-to-list 'load-path "~/.emacs.d/lisps/popup-kill-ring/")
;;(require 'popup)
(require 'pos-tip)
(require 'popup-kill-ring)

;;(global-set-key "\M-y" 'popup-kill-ring)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; 使用 Ctrl-x r j <char> 就可以進行快速跳轉至檔案，其中 <char> 為以下所設。
(require 'cl)
(dolist
    (r `(
         (?e (file . "~/.emacs.d/init.el"))
         ))
  (set-register (car r) (cadr r)))

;; (defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
(setq helm-ack-grep-executable "ack")
(setq helm-grep-default-command "ack -Hn --smart-case --nogroup --nocolour %e %p %f")
(setq helm-grep-default-recurse-command "ack -H --smart-case --nogroup --nocolour %e %p %f")


(global-set-key (kbd "C-x <f2>") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<f2>") 'kmacro-end-or-call-macro)

;;自動啟動flyspell-mode拼字檢查
;;(setq-default flyspell-mode t)
;;flyspell-prog-mode是為程式設計師的輔模式，Emacs将只在注释和字符串里高亮错误的拼写。
;;(setq-default flyspell-prog-mode t)
(global-set-key (kbd "C-x <f3>") 'flyspell-mode)
(global-set-key (kbd "C-c <f3>") 'flyspell-buffer)
(global-set-key (kbd "<f3>") 'flyspell-check-previous-highlighted-word)
(global-set-key (kbd "C-x <f4>") 'ispell-buffer)
(global-set-key (kbd "<f4>") 'ispell-word) ;;M-$，有夠難記，很容易跟query-replace的M-%搞混


(global-set-key (kbd "<f9>") 'open-note)
(defun open-note ()
  "Open stick note."
  (interactive)(find-file (concat org-directory "/notes.org")))

(global-set-key (kbd "C-x <f9>") 'open-computer-notes)
(defun open-computer-notes ()
  "open-computer-notes"
  (interactive)(find-file (concat org-directory "/computer_notes.org")))

(global-set-key (kbd "<f10>") 'open-agenda-directory)
(defun open-agenda-directory ()
  "Open the directory of Org agenda files."
  (interactive)(find-file (concat org-directory "/agenda")))

(global-set-key (kbd "C-x <f10>") 'open-nihongo-note)
(defun open-nihongo-note ()
  "Open nihongo note."
  (interactive)(find-file (concat org-directory "/日本語のノート.org")))

(global-set-key (kbd "C-x <f11>") 'open-diary)
(defun open-diary ()
  "Open diary."
  (interactive)(find-file (concat org-directory "/diary/diary.org")))

(global-set-key (kbd "<f11>") 'open-material-notes)
(defun open-material-notes ()
  "Open material notes."
  (interactive)(find-file (concat org-directory "/materials.org")))

(defun open-blog-dir ()
  (interactive)(find-file "~/Dropbox/Blog"))
(global-set-key (kbd "C-x <f12>") 'open-blog-dir)

(global-set-key (kbd "<f11>") (lambda () (interactive) (other-frame 1)))
(global-set-key (kbd "<f12>") (lambda () (interactive) (other-frame -1)))

;; StarDict
;; please install sdcv on your system first
(global-set-key (kbd "C-c d s") 'stardict-lookup)
(defun stardict-lookup ()
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (if mark-active
        (setq begin (region-beginning)
              end (region-end))
      (save-excursion
        (backward-word)
        (mark-word)
        (setq begin (region-beginning)
              end (region-end))))
    (message "searching for %s ..." (buffer-substring begin end))
    (with-output-to-temp-buffer "*Stardict*"
      (prin1 (shell-command-to-string
              (concat "sdcv -n "
                      (buffer-substring begin end)))))
    (switch-to-buffer-other-window "*Stardict*")
    (let (buffer-read-only)
      (delete-region (- (point-max) 2) (point-max))
      (delete-region 1 2)
      (font-lock-fontify-buffer))))

(font-lock-add-keywords 'help-mode
                        '(("^-->.*\n" . 'moedict-bopomofo)
                          ("<<.+>>" . 'bold)
                          ("\\(noun\\|verb\\|adjective\\|adverb\\|adj[ s\n]\\|adv [ s\n]\\)" . 'font-lock-constant-face)))

;; (require 'sdcv-mode)
;; (global-set-key (kbd "C-c s") 'sdcv-search)

;; 沒屁用的C-z 改成mark
(global-set-key (kbd "C-z") 'set-mark-command)

;;======================================================
;; Tmux 相關設定
;;======================================================

(defun zsh () (interactive) (term "/bin/zsh"))

;;解決tmux下無法切換buffer以及一些key-binding的問題
(global-set-key (kbd "C-x M-[ d") 'previous-buffer)
(global-set-key (kbd "C-x M-[ c") 'next-buffer)
(global-set-key (kbd "M-[ c") 'forward-word)
(global-set-key (kbd "M-[ d") 'backward-word)
(global-set-key (kbd "C-c M-[ d") 'backward-sexp)
(global-set-key (kbd "C-c M-[ c") 'forward-sexp)
(global-set-key (kbd "C-c M-[ a") 'backward-up-list)
(global-set-key (kbd "C-c M-[ b") 'down-list)

;;下面這幾個原本的binding不好按或者會跟kwin衝。
(global-set-key (kbd "C-c <C-left>") 'backward-sexp)
(global-set-key (kbd "C-c <C-right>") 'forward-sexp)
(global-set-key (kbd "C-c <C-up>") 'backward-up-list)
(global-set-key (kbd "C-c <C-down>") 'down-list)

(defun eval-buffer-and-message ()
  (interactive)
  (eval-buffer)
  (message "Eval done!"))

(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-buffer-and-message) ;;這樣測試.emacs方便多了...

;; [FIXME] 這不知是啥
;;(load-file "~/.emacs.d/lisps/copypaste.el")

(cond ((eq system-type 'darwin)
       ;; ======================================================
       ;; Mac OS X
       ;; ======================================================
       (defun cp ()
         (interactive)
         (call-process-region (point) (mark) "pbcopy")
         (setq deactivate-mark t))
       (defun paste ()
         (interactive)
         (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t))

       (defun cut ()
         (interactive)
         (cp)
         (delete-region (region-beginning) (region-end)))

       ;; (defun send-killring-to-osx (text &optional push)
       ;;   (let ((process-connection-type nil))
       ;;     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
       ;;       (process-send-string proc text)
       ;;       (process-send-eof proc))))

       ;; (defun get-clipboard-from-osx ()
       ;;   (shell-command-to-string "pbpaste"))

       ;; (setq interprogram-cut-function 'send-killring-to-osx)
       ;; (setq interprogram-paste-function 'get-clipboard-from-osx)
       )
      ((eq system-type 'gnu/linux)
       ;; ======================================================
       ;; Linux
       ;; ======================================================
       (setq x-select-enable-clipboard t)
       (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
       (defun cp ()
         (interactive)
         (if (region-active-p)
             (progn
               (shell-command-on-region (region-beginning) (region-end) "xsel -i")
               (message "Yanked region to clipboard!")
               (deactivate-mark))
           (message "No region active; can't yank to clipboard!")))

       ;;xclip-mode
       (load "~/.emacs.d/lisps/xclip-1.0.el")
       (define-minor-mode xclip-mode
         "Minor mode to use the `xclip' program to copy&paste."
         :global t
         (if xclip-mode
             (turn-on-xclip)
           (turn-off-xclip)))
       (turn-off-xclip)
       ))


;;======================================================
;; Frames 操作加強
;;======================================================
;; smart-window.el
(add-to-list 'load-path "~/.emacs.d/lisps/smart-window/")
(require 'smart-window)
;;(setq smart-window-remap-keys 0)
(global-set-key (kbd "C-x w") 'smart-window-move)
(global-set-key (kbd "C-x W") 'smart-window-buffer-split)
(global-set-key (kbd "C-x M-w") 'smart-window-file-split)
(global-set-key (kbd "C-x R") 'smart-window-rotate)
(global-set-key (kbd "C-x 2") 'sw-below)
(global-set-key (kbd "C-x 3") 'sw-right)

;;switch frames in a visual way (C-x o)
;; (require 'switch-window)

;; resize frame easily
(require 'resize-frame)

;;======================================================
;; Emacs 本身key-binding改進
;;======================================================

;;discover-mode
(global-discover-mode 1)


;;有時會按錯C-x C-c，所以叫Emace確認後再關掉！
(setq confirm-kill-emacs 'yes-or-no-p)

;; C-z 太常按錯了，直接關掉這binding
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))


;;======================================================
;; Emacs 自動備份 Auto backup
;;======================================================

;; Emacs會自動給你編輯的檔案備份（以~為結尾，例如編輯test，會產生test~）
;; 然而這樣常常會把目錄弄得很亂，所以以下設定可以讓這些備份檔統統塞
;; 進~/.saves中，保持屁屁和工作目錄的乾爽。

(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; ======================================================
;; God-mode
;; ======================================================

;; (global-set-key (kbd "M-z") 'god-local-mode)

;;======================================================
;; Shorten indicators in Mode-line
;;======================================================

(require 'rich-minority)
(rich-minority-mode 1)
(setf rm-blacklist "")

(defface projectile-mode-line
  '((((class color) (background light)) (:foreground "#444" :bold t))
    (((class color) (background dark)) (:foreground "#444" :bold t)))
  "Face for title. ex:"
  :group 'projectile-faces)

(setq projectile-mode-line '(:eval
                             (propertize
                              (if (file-remote-p default-directory) " Pj" (format " %s" (projectile-project-name)))
                              'face 'projectile-mode-line
                              )))

(setq-default mode-line-format
              '((god-local-mode (:eval (propertize " G " 'face 'compilation-error)))
                "%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification mode-line-position
                (vc-mode vc-mode) " "
                mode-line-modes projectile-mode-line mode-line-misc-info mode-line-end-spaces))

(setq mode-line-position
      `((1 ,(propertize
             " %p"
             'local-map mode-line-column-line-number-mode-map
             'mouse-face 'mode-line-highlight
             ;; XXX needs better description
             'help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu"))
        (size-indication-mode
         (2 ,(propertize
              "/%I"
              'local-map mode-line-column-line-number-mode-map
              'mouse-face 'mode-line-highlight
              ;; XXX needs better description
              'help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu")))
        (line-number-mode
         ((column-number-mode
           (1 ,(propertize
                "(%l,%c)"
                'local-map mode-line-column-line-number-mode-map
                'mouse-face 'mode-line-highlight
                'help-echo "Line number and Column number\n\
mouse-1: Display Line and Column Mode Menu"))
           (1 ,(propertize
                "L%l"
                'local-map mode-line-column-line-number-mode-map
                'mouse-face 'mode-line-highlight
                'help-echo "Line Number\n\
mouse-1: Display Line and Column Mode Menu"))))
         ((column-number-mode
           (1 ,(propertize
                "C%c"
                'local-map mode-line-column-line-number-mode-map
                'mouse-face 'mode-line-highlight
                'help-echo "Column number\n\
mouse-1: Display Line and Column Mode Menu"))))))
      )

(global-set-key [f6] 'point-to-register)
(global-set-key [f7] 'jump-to-register)

(define-key Info-mode-map "q" #'Info-up)
(define-key Info-mode-map "Q" #'quit-window)


;; ======================================================
;; Jump to *Help* window/buffer quickly
;; ======================================================

(global-set-key (kbd "C-h SPC") 'help-jump-to-help-window)
(defun help-jump-to-help-window ()
  (interactive)
  (switch-to-buffer "*Help*"))

;; Remap some keys
(add-hook 'help-mode-hook (lambda ()
                            (define-key help-mode-map (kbd "[") 'help-go-back)
                            (define-key help-mode-map (kbd "]") 'help-go-forward)
                            (define-key help-mode-map (kbd "q") 'help-go-back)
                            (define-key help-mode-map (kbd "Q") 'quit-window)))

;; ======================================================
;; Tramp
;; ======================================================

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (find-file (format "/sudo::%s" (read-file-name "Find file as root: "))))


(provide 'rc-basic)
;;; basic.el ends here
