;;; rc-org.el ---                                    -*- lexical-binding: t; -*-
;;======================================================
;; Org-mode
;;======================================================
;;(delete "/usr/local/share/emacs/24.4/lisp/org" load-path)
;;(add-to-list 'load-path "~/.emacs.d/git/org-mode/lisp")
(require 'org-install)
(require 'org)
(require 'org-habit)
(require 'ox)
(require 'ox-md)
(require 'ox-html5slide)
(require 'ox-odt)
(setq org-directory "~/org")
(define-key global-map "\C-cl" 'org-store-link)
(setq org-log-done t)

(global-set-key (kbd "C-c !") 'org-time-stamp-inactive)

;; In tmux/tty, M-S-RET will be inpretered to this shit.
(define-key org-mode-map (kbd "ESC <kp-enter>") 'org-insert-todo-heading)

(setq org-display-table t)
(setq org-display-inline-images t)
;;讓org中顯示圖片能夠先用imagemagick自動縮放
(setq org-image-actual-width '(300)) ;; 可以設定成一個數字
;;解決org-mode下中文不自動換行的問題
(add-hook 'org-mode-hook
          (lambda () (setq truncate-lines nil)))

;;org-mode裡的項目變成done時會自動加上CLOSED: [timestamp]戳記；改成'note為筆記
(setq org-log-done 'time)
;;(setq org-log-done 'note)
(defun org-insert-bold ()
  "Insert *bold* at cursor point."
  (interactive)
  (insert " ** ")
  (backward-char 2))
(define-key org-mode-map (kbd "C-c b") 'org-insert-bold)

;;(setq org-export-default-language "zh"
;;      org-export-html-extension "html"
;;      org-export-with-timestamps nil
;;      org-export-with-section-numbers t
;;      org-export-with-tags 'not-in-toc
;;      org-export-skip-text-before-1st-heading nil
;;      org-export-with-sub-superscripts '{}
;;      org-export-with-LaTeX-fragments t
;;      org-export-with-archived-trees nil
;;      org-export-highlight-first-table-line t
;;      org-export-latex-listings-w-names nil
;;      org-html-head-include-default-style nil
;;      org-html-head ""
;;      org-export-htmlize-output-type 'css
;;      org-startup-folded nil
;;      org-export-allow-BIND t
;;      org-publish-list-skipped-files t
;;      org-publish-use-timestamps-flag t
;;      org-export-babel-evaluate nil
;;      org-confirm-babel-evaluate nil)

;;輸出上下標？
;;(setq org-export-with-sub-superscripts nil)

(setq org-file-apps '((auto-mode . emacs)
                      ("\\.mm\\'" . default)
                      ("\\.x?html?\\'" . "xdg-open %s")
                      ("\\.pdf\\'" . "kde-open %s")
                      ("\\.jpg\\'" . "kde-open %s")))
;; Syntax Highlight in outputed files
(setq org-src-fontify-natively t)

(setq org-html-style "<style type=\"text/css\">
* {
    font-family:WenQuanYi Micro Hei,Microsoft JhengHei,Helvetica,sans-serif;
    color: #555555;
    }

body {
    text-align: center;
    background-color: hsl(45,30%,80%);
    background-image:
    repeating-linear-gradient(120deg, rgba(255,255,255,.1), rgba(255,255,255,.1) 2px, transparent 1px, transparent 60px),
    repeating-linear-gradient(60deg, rgba(255,255,255,.1), rgba(255,255,255,.1) 2px, transparent 1px, transparent 60px),
    linear-gradient(60deg, rgba(0,0,0,.06) 25%, transparent 25%, transparent 75%, rgba(0,0,0,.06) 75%, rgba(0,0,0,.06)),
    linear-gradient(120deg, rgba(0,0,0,.06) 25%, transparent 25%, transparent 75%, rgba(0,0,0,.06) 75%, rgba(0,0,0,.06));
    background-size: 70px 120px;
}

#content {
    margin: 0px auto;
    width: 1200px;
    text-align:left;
    background-color: rgba(255, 255, 255, 1);
    border-radius: 7px 7px 0 0;
    box-shadow: 0 0 0.5em rgba(0,0,0,0.2);
    padding-bottom: 30px;
}

#content > p {
    padding-left: 60px;
}


#postamble {
    position: relative;
    z-index: 1;
    font-size:0.8em;
    color: #ffffff !important;
    background-color: #555555 !important;
    margin: -1.2em auto 0;
    width: 1180px;
    background-color: #ffffff;
    border-radius: 0 0 7px 7px;
    padding: 30px 10px 10px 10px;
    box-shadow: 0 0 0.5em rgba(0,0,0,0.2);
    text-shadow: 0px -1px rgba(0, 0, 0, 0.3);

    background-color: #828282;
    background-image: radial-gradient(#707070 50%, transparent 51%);
    background-size: 4px 4px;
}
#postamble p {
    margin: 0;
    color: #eeeeee !important;
}

#postamble a {
    color: #5fafd7;
}

#table-of-contents {
    margin: 0 30px;
    padding: 0 15px 10px 15px;
    border-top: 4px solid #D4DDE0;
    border-bottom: 4px solid #D4DDE0;
    background-color: #E9EEF1;
    text-shadow: 0 1px 0 hsl(202,100%,100%);
}

#table-of-contents h2 {
    color: hsl(202,40%,52%);
    text-shadow: 0 1px 0 hsl(202,100%,100%);
    border-left: none;
    margin-left: 0;
    padding-left: 0;
}

img {
    max-width:100%;
    max-height:100%;
}
a {
    color: #005f87;
    text-decoration: none;
}

a:hover {
    color: #005f87;
    text-decoration: underline;
}

h1 {
    color: #eeeeee;
    text-shadow: 0px -1px rgba(0, 0, 0, 0.5);
    font-family: Lato,Lucida Grande,LiHei Pro,WenQuanYi Micro Hei,Arial,sans-serif;
    font-weight: 400;
    margin-top: 0px;
    padding: 20px 0 10px 0;
    background-color: #828282;
    background-image: radial-gradient(#707070 50%, transparent 51%);
    background-size: 4px 4px;
    border-radius: 7px 7px 0 0;
}
h2 {
    color: #777;
    border-left: 5px solid #777;
    margin-left: -30px;
    padding-left: 25px;
}

.outline-2 { padding: 0px 30px; }
.outline-3 { padding: 0px 30px; }

.outline-text-2 { padding: 0px 0px; }
.outline-text-3 { padding: 0px 0px; }
.example { }
pre {
    border: 1pt solid #ddd;
    background-color: #f2f2f2;
    box-shadow: 0 0 1em rgba(0,0,0,0.05);
    border-radius:5px;
    padding: 5pt;
    font-family: courier, monospace;
    font-size: 90%;
    overflow:auto;
    margin: 0.5em 2em;
}

pre.src:before {
    background-color: rgba(0, 0, 0, 0.5);
    color: #fff;
    border-radius: 5px;
    border: none;
    top: -10px;
    right: 10px;
    padding: 3px 7px;
    position: absolute;
}

code {
    border: 1pt solid #ddd;
    background-color: #eee;
    padding: 0 3px;
    border-radius: 3px;
    position: relative;
    margin-top: -3px;
    font-family: courier, monospace;
    font-size: 80%;
}

blockquote {
    font-style:italic;
    background: hsl(44,80%,95%);
    border-left: 5px solid hsl(44,25%,70%);
    margin: 1.5em 2em;
    padding: 0.5em 10px 0.5em 4em;
    quotes: '\\201C''\\201D''\\2018''\\2019';
}
blockquote:before {
    color: #ccc;
    position: absolute;
    margin-top: -0.03em;
    margin-left: -1.3em;
    color: hsl(44,25%,85%);
    font-size: 5em;
    content: '\\201C' !important;
}
blockquote p {
    display: inline;
    font-family:'Times New Roman', Times, serif !important;
}
blockquote p a {
    font-family:'Times New Roman', Times, serif !important;
      }

.done {
    background-color: #d7ff87;
    color: #008700;
    border: 1px solid #5faf00;
    border-radius: 3px;
    padding:0px 2px;
    top: -1px;
    position: relative;
    font-family:WenQuanYi Micro Hei,Microsoft JhengHei,Helvetica,sans-serif;
    font-weight: bold;
    font-size:0.8em;
}
.todo {
    background-color: #ffafaf;
    color: #a40000;
    border: 1px solid #dd0000;
    border-radius: 3px;
    padding:0px 2px;
    top: -1px;
    position: relative;
    font-family:WenQuanYi Micro Hei,Microsoft JhengHei,Helvetica,sans-serif;
    font-weight: bold;
    font-size:0.8em;
}
.tag { float:right; color:red; }

h2.footnotes {
    margin-left: 0;
}
#text-footnotes {
    margin-left: 30px;
}

</style>")

;;(add-function :override org-html-checkbox
;;(defun org-html-checkbox (checkbox)
;;  "Format CHECKBOX into HTML."
;;  (case checkbox (on "<code>[X]</code>")
;;    (off "<code>[&#xa0;]</code>")
;;    (trans "<code>[-]</code>")
;;    (t "")))


;; org輸出html時中文不要有奇怪的空白。（by coldnew the God）
;; Avoid unnecessary/wrong spaces when export to HTML.
(defadvice org-html-paragraph (before org-html-paragraph-advice
                                      (paragraph contents info) activate)
  "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
  (let* ((origin-contents (ad-get-arg 1))
         (fix-regexp "[[:multibyte:]]")
         (fixed-contents
          (replace-regexp-in-string
           (concat
            "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))

    (ad-set-arg 1 fixed-contents)))

;; Export UTF-8 checkboxes
;; This snippet turns - [X] into ☑ and - [ ] into ☐.
(defun sacha/org-html-checkbox (checkbox)
  "Format CHECKBOX into HTML."
  (case checkbox
    (on "<span class=\"check\">&#x2611;</span>") ; checkbox (checked)
    (off "<span class=\"checkbox\">&#x2610;</span>")
    (trans "<code>[-]</code>")
    (t "")))
(defadvice org-html-checkbox (around sacha activate)
  (setq ad-return-value (sacha/org-html-checkbox (ad-get-arg 0))))

;; To follow links with RET, rather than a 2 key combo:
(setq org-return-follows-link t)

;; 指定agenda檔案位置清單
(setq org-agenda-files (list (concat org-directory "/agenda/Todo.org")))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "<f11>") 'org-agenda)

(setq org-log-into-drawer t)
(setq org-log-reschedule 'note)
(setq org-log-redeadline t)
(setq org-log-done 'time)
(setq org-todo-keywords
      '((type "TODO(t!)" "STARTED(s!)" "WAITING(w!)" "APPT(a!)" "|" "DONE(d!)")
        (type "PROJECT(p!)" "|" "DONE(d!)")
        (type "|" "CANCELLED(x@)" "DEFERRED(f@)")))



;;;;;;;;;;;;AGENDA~~~~~~ =w="
;;Including all org files from a directory into the agenda
;;(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
;; 啊啊啊啊Agenda自訂
;; shortcut可以一個字母以上
;; Example:  http://doc.norang.ca/org-mode.html#CustomAgendaViewSetup



;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
;; Compact the block agenda view
(setq org-agenda-compact-blocks nil);; nil為加上分隔線，t為去掉
;; 用describe-char來查你想要的seperator char code
(setq org-agenda-block-separator 45)

;; (setq org-stuck-projects
;;       '("TODO=\"PROJECT\""
;;         ("ACTION" "WAITING")
;;         nil
;;         nil))

;; Function to skip tag
;; From http://stackoverflow.com/questions/10074016/org-mode-filter-on-tag-in-agenda-view
(defun ky/org-agenda-skip-tag (tag &optional others)
  "Skip all entries that correspond to TAG.

If OTHERS is true, skip all entries that do not correspond to TAG."
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (if others
        (if (not (member tag (org-get-tags-at current-headline)))
            next-headline
          nil)
      (if (member tag (org-get-tags-at current-headline))
          next-headline
        nil))))

(setq org-agenda-custom-commands
      '(
        ("w" todo "STARTED") ;; (1) (3) (4)
        ;; ...other commands here

        ("D" "Daily Action List"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)
                      ))))

        ("P" "Projects"
         ((tags "Project")))
        (" " "Agenda"
         ((todo "STARTED"
                ((org-agenda-overriding-header "What you should doing right now!")
                 (org-tags-match-list-sublevels nil)))
          (todo "WAITING"
                ((org-agenda-overriding-header "Things waiting on the perenially disorganised masses")
                 (org-tags-match-list-sublevels nil)))

          (agenda "Timetable, diary & date tasks" ((org-agenda-ndays 7)
                                                   (org-deadline-warning-days 45))) ;; review upcoming deadlines and appointments
          ;;          (stuck "") ;; review stuck projects as designated by org-stuck-projects
          (todo ""
                ((org-agenda-overriding-header "All other TODOs")
                 (org-agenda-todo-ignore-scheduled t)
                 (org-agenda-todo-ignore-deadlines t)
                 (org-agenda-todo-ignore-with-date t)
                 (org-agenda-todo-ignore-timestamp t)
                 (org-agenda-skip-function '(ky/org-agenda-skip-tag "Project"))
                 ))
          (tags-todo "Project" ((org-agenda-overriding-header "Projects' TODOs")))
          )) ;; review waiting items
        ;; ...other commands here

        ("d" "Upcoming deadlines" agenda ""
         ((org-agenda-entry-types '(:deadline))
          ;; a slower way to do the same thing
          ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
          (org-agenda-ndays 1)
          (org-deadline-warning-days 60)
          (org-agenda-time-grid nil)))
        ;; ...other commands here

        ("c" "Weekly schedule" agenda ""
         ((org-agenda-ndays 7)          ;; agenda will start in week view
          (org-agenda-repeating-timestamp-show-all t)   ;; ensures that repeating events appear on all relevant dates
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
        ;; limits agenda view to timestamped items
        ;; ...other commands here

        ("P" "Printed agenda"
         ((agenda "" ((org-agenda-ndays 7)                      ;; overview of appointments
                      (org-agenda-start-on-weekday nil)         ;; calendar begins today
                      (org-agenda-repeating-timestamp-show-all t)
                      (org-agenda-entry-types '(:timestamp :sexp))))
          (agenda "" ((org-agenda-ndays 1)                      ;; daily agenda
                      (org-deadline-warning-days 7)             ;; 7 day advanced warning for deadlines
                      (org-agenda-todo-keyword-format "[ ]")
                      (org-agenda-scheduled-leaders '("" ""))
                      (org-agenda-prefix-format "%t%s")))
          (todo "TODO"                                          ;; todos sorted by context
                ((org-agenda-prefix-format "[ ] %T: ")
                 (org-agenda-sorting-strategy '(tag-up priority-down))
                 (org-agenda-todo-keyword-format "")
                 (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
         ((org-agenda-with-colors nil)
          (org-agenda-compact-blocks t)
          (org-agenda-remove-tags t)
          (ps-number-of-columns 2)
          (ps-landscape-mode t))
         ("~/agenda.ps"))
        ;; other commands go here
        ))

(setq org-refile-targets '(("Todo.org" :maxlevel . 1)
                           ("School.org" :maxlevel . 1)
                           ("Learning.org" :maxlevel . 1)
                           ("Project.org" :maxlevel . 2)
                           ("Event.org" :maxlevel . 1)
                           ("Reading.org" :maxlevel . 1)))


;;To save the clock history across Emacs sessions, use

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
;; Use a drawer to place clocking info
(setq org-clock-into-drawer t)
;; Global clocking key
(global-set-key (kbd "C-c C-x C-x") 'org-clock-in-last)
(global-set-key (kbd "C-c C-x C-i") 'org-clock-in)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(add-to-list 'recentf-exclude ".+org-clock-save\\.el$")
                                        ;Now that OrgMode and RememberMode are included in Emacs (as of Emacs 23), activation is as simple as:
;;(org-remember-insinuate)
;;This excellent feature inspired Capture in OrgMode and that is now (Aug2010) recommended for new users, see http://orgmode.org/manual/Capture.html#Capture

;;Org-Capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;; [Agenda]
(global-set-key (kbd "<f12>") (lambda () (interactive) (org-agenda nil " ")))
;; [Capture]
(global-set-key (kbd "<f11>") (lambda () (interactive) (org-capture)))
;; [Capture] Diary+Timer
(global-set-key (kbd "ESC <f11>") (lambda () (interactive) (org-capture nil "D")))


(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline (concat org-directory "/agenda/Todo.org") "Todo")
         "** TODO %? %^G\n  Created: %U \n  %i")
	("s" "School" entry
	 (file+headline (concat org-directory "/agenda/School.org") "School")
	 "** TODO %?\n  Created: %U \n  %i")
	("r" "Reading" entry
	 (file+headline (concat org-directory "/agenda/Reading.org") "Reading")
	 "** %? %i :Reading:\n  Created: %U")
	("D" "Diary + Timer" entry
	 (file+datetree (concat org-directory "/diary/diary.org"))
	 "* %^{Description: } %^g  \n  %i %?\n" :clock-in t :clock-keep t)
	("d" "Diary" entry
	 (file+datetree (concat org-directory "/diary/diary.org"))
	 "* %? \n  Created: %U \n")
	("e" "Event" entry
	 (file+headline (concat org-directory "/agenda/Event.org") "Event")
	 "** %? %^g\n%^{Event's date&time? }T\n  %i")))

;; I set my capture for diary like this:
;; ("d" "Diary" entry  (file+datetree (concat org-directory "/diary/diary.org")) "* %^{Description: } %^g  \n  %i %?\n" :clock-in t :clock-keep t)
;;but it create duplicated "datetree" every time call this capture http://paste.opensuse.org/21805084 Any body know what's happened?

(setq cfw:org-capture-template
      '("c" "calfw2org" entry
        (file nil)
        "** %?\n %(cfw:org-capture-day)"))

;; capture jump to link
(define-key global-map "\C-cx"
  (lambda () (interactive) (org-capture nil "x")))

;; used by org-clock-sum-today-by-tags
(defun filter-by-tags ()
  (let ((head-tags (org-get-tags-at)))
    (member current-tag head-tags)))

;; 每日時間統計
;; http://www.mastermindcn.com/2012/02/org_mode_quite_a_life/
(defun org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("ACADEMIC" "ENGLISH" "SCHOOL"
                         "LEARNING" "OUTPUT" "OTHER"))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (tstart (or tstart
                     (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                     (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                     (org-time-today)))
         (tend (or tend
                   (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                   (+ tstart 86400)))
         h m file item prompt donesomething)
    (while (setq file (pop files))
      (setq org-agenda-buffer (if (file-exists-p file)
                                  (org-get-agenda-file-buffer file)
                                (error "No such file %s" file)))
      (with-current-buffer org-agenda-buffer
        (dolist (current-tag include-tags)
          (org-clock-sum tstart tend 'filter-by-tags)
          (setcdr (assoc current-tag tags-time-alist)
                  (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
    (while (setq item (pop tags-time-alist))
      (unless (equal (cdr item) 0)
        (setq donesomething t)
        (setq h (/ (cdr item) 60)
              m (- (cdr item) (* 60 h)))
        (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
    (unless donesomething
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert
      (insert output-string))
    output-string))

(define-key org-mode-map (kbd "C-c C-x t") 'org-clock-sum-today-by-tags)


(setq org-latex-classes
      '(("article"
         "
\\documentclass[12pt,a4paper]{article}
\\usepackage[margin=2cm]{geometry}
\\usepackage{fontspec}
\\setromanfont{cwTeXMing}

\\usepackage{etoolbox}  % Quote部份的字型設定
\\newfontfamily\\quotefont{cwTeXFangSong}
\\AtBeginEnvironment{quote}{\\quotefont\\small}

\\setmonofont[Scale=0.9]{Courier} % 等寬字型 [FIXME] Courier 中文會爛掉！
\\font\\cwSong=''cwTeXFangSong'' at 10pt
%\\font\\cwHei=''cwTeXHeiBold'' at 10p %不知為何會爆掉
\\font\\cwYen=''cwTeXYen'' at 10pt
\\font\\cwKai=''cwTeXKai'' at 10pt
\\font\\cwMing=''cwTeXMing'' at 10pt
\\font\\wqyHei=''文泉驛正黑'' at 10pt
\\font\\wqyHeiMono=''文泉驛等寬正黑'' at 10pt
\\font\\wqyHeiMicro=''文泉驛微米黑'' at 10pt
\\XeTeXlinebreaklocale ``zh''
\\XeTeXlinebreakskip = 0pt plus 1pt
\\linespread{1.36}

\\usepackage{multicol}

% [FIXME] ox-latex 的設計不良導致hypersetup必須在這裡插入
\\usepackage{hyperref}
\\hypersetup{
  colorlinks=true, %把紅框框移掉改用字體顏色不同來顯示連結
  linkcolor=[rgb]{0,0.37,0.53},
  citecolor=[rgb]{0,0.47,0.68},
  filecolor=[rgb]{0,0.37,0.53},
  urlcolor=[rgb]{0,0.37,0.53},
  pagebackref=true,
  linktoc=all,}
"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

        ("beamer"
         "
\\documentclass[presentation]{beamer}
\\usepackage{fontspec}
\\setromanfont{wqyHeiMicro}

\\setmonofont[Scale=0.9]{Courier} % 等寬字型 [FIXME] Courier 中文會爛掉！
\\font\\cwSong=''cwTeXFangSong'' at 10pt
%\\font\\cwHei=''cwTeXHeiBold'' at 10p %不知為何會爆掉
\\font\\cwYen=''cwTeXYen'' at 10pt
\\font\\cwKai=''cwTeXKai'' at 10pt
\\font\\cwMing=''cwTeXMing'' at 10pt
\\font\\wqyHei=''文泉驛正黑'' at 10pt
\\font\\wqyHeiMono=''文泉驛等寬正黑'' at 10pt
\\font\\wqyHeiMicro=''文泉驛微米黑'' at 10pt
\\XeTeXlinebreaklocale ``zh''
\\XeTeXlinebreakskip = 0pt plus 1pt
\\linespread{1.36}

"
         ("\\section{%s}" . "\\section*{%s}")
         ("\\subsection{%s}" . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}" . "\\paragraph*{%s}")
         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
        ))


;; [FIXME]
;; 原本是不要讓org插入hypersetup（因為org-mode這部份設計成沒辦法自訂，或許可以去report一下？）
;; 改成自行插入，但這樣pdfcreator沒辦法根據Emacs版本插入，pdfkeyword也會無效...幹。
(setq org-latex-with-hyperref t)

;; 把預設的fontenc拿掉
;; 經過測試XeLaTeX輸出PDF時有fontenc[T1]的話中文會無法顯示。
;; hyperref也拿掉，改從classes處就插入，原因見上面 org-latex-with-hyperref 的說明。
(setq org-latex-default-packages-alist
      '(("" "hyperref" nil)
        ("AUTO" "inputenc" t)
        ("" "fixltx2e" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "marvosym" t)
        ("" "wasysym" t)
        ("" "amssymb" t)
        "\\tolerance=1000"))


;; Use XeLaTeX to export PDF in Org-mode
(setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))
;;;; Dependancies: wrapfig
;;;;(setq org-latex-default-class "ltjsarticle")
;;;;(setq org-latex-pdf-process '("lualatex %b" "lualatex %b"))

(require 'ob-latex)
(setq org-src-fontify-natively t)

;; Automatically generate LaTeX preview picture.
(define-key org-mode-map (kbd "$") (lambda ()
                                     (interactive)
                                     (insert "$")
                                     (save-excursion
                                       (left-char 1)
                                       (if (org-inside-LaTeX-fragment-p)
                                           (progn
                                             (right-char 2)
                                             (org-preview-latex-fragment))))))


(require 'ox-html5slide)

(add-to-list 'load-path "~/.emacs.d/lisps/org-ioslide/")
(require 'ox-ioslide)

;;======================================================
;; LaTeX
;;======================================================
;;(add-to-list 'tex-compile-commands '("xelatex %r"))
(setq tex-compile-commands '(("xelatex %r")))
(setq tex-command "xelatex")

(setq-default TeX-engine 'xelatex)
(setq TeX-command-list
      '(("TeX" "%(PDF)%(tex) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
         (plain-tex-mode ams-tex-mode texinfo-mode)
         :help "Run plain TeX")
        ("LaTeX" "xelatex -interaction nonstopmode %t" TeX-run-TeX nil
         (latex-mode doctex-mode)
         :help "Run LaTeX")
        ("Makeinfo" "makeinfo %t" TeX-run-compile nil
         (texinfo-mode)
         :help "Run Makeinfo with Info output")
        ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
         (texinfo-mode)
         :help "Run Makeinfo with HTML output")
        ("AmSTeX" "%(PDF)amstex %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
         (ams-tex-mode)
         :help "Run AMSTeX")
        ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
         (context-mode)
         :help "Run ConTeXt once")
        ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
         (context-mode)
         :help "Run ConTeXt until completion")
        ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
        ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
        ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
        ("Print" "%p" TeX-run-command t t :help "Print the file")
        ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
        ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
        ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
        ("Check" "lacheck %s" TeX-run-compile nil
         (latex-mode)
         :help "Check LaTeX file for correctness")
        ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
        ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
        ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
        ("Other" "" TeX-run-command t t :help "Run an arbitrary command")))

;;======================================================
;; calfw - Calendar Framework
;;======================================================

(require 'calfw)
(require 'calfw-org)
(require 'calfw-cal)
(global-set-key (kbd "C-c A") 'my-cfw:open-org-calendar)
(define-key org-agenda-mode-map (kbd "A") 'my-cfw:open-org-calendar)
;; 解決開啟cfw後，原buffer中的cursor會自己莫名其妙亂跳走的怪問題。
(defun my-cfw:open-org-calendar ()
  (interactive)
  (let ((currentBuf (current-buffer))
        (currentPos (point)))
    (cfw:open-org-calendar)
    (switch-to-buffer currentBuf)
    (goto-char currentPos))
  (switch-to-buffer "*cfw-calendar*"))

;; 吃太飽的話可以自己去定calendar-holidays
;; Month
(setq calendar-month-name-array
  ["January" "February" "March"     "April"   "May"      "June"
   "July"    "August"   "September" "October" "November" "December"])
;; Week days
(setq calendar-day-name-array
      ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])
;; First day of the week
(setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday



(defun org-qt4-add-doc-link ()
  (interactive)
  (let* ((begin (progn (right-char 1) (backward-word 1) (point)))
         (end (progn (forward-word 1) (point)))
         (Q (buffer-substring-no-properties begin end)))
    (if (string-match "^Q[A-z0-9]+" Q)
        (progn
          (delete-region begin end)
          (insert
           (format "[[http://qt-project.org/doc/qt-4.8/%s.html][%s]]"
                   (downcase Q) Q)))
      (message "This seems not to belong to Qt namespace"))))
(define-key org-mode-map (kbd "C-c i q") 'org-qt4-add-doc-link)


(provide 'rc-org)
;;; rc-org.el ends here
