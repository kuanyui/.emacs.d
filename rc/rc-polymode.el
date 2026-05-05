;;; rc-polymode.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2026  ono

(require 'polymode)

;; ======================================================
;; Host Mode
;; ======================================================

(define-hostmode poly-dockerfile-hostmode
  :mode 'dockerfile-mode)

(define-hostmode poly-elisp-hostmode
  :mode 'emacs-lisp-mode)

(define-hostmode poly-sh-hostmode
  :mode 'sh-mode)

(define-hostmode poly-apparmor-hostmode
  :mode 'apparmor-mode)

;; ======================================================
;; Inner Mode
;; ======================================================


(define-innermode poly-jinja2-innermode
  :mode 'jinja2-mode
  :head-matcher "{[{%]"
  :tail-matcher "[}%]}"
  :head-adjust-face '(:foreground "#ee88ff" :weight bold)
  :tail-adjust-face '(:foreground "#ee88ff" :weight bold)
  :fallback-mode 'host)

(define-innermode poly-jinja2-raw-innermode
  :mode nil                             ; use (inherit) host mode
  :head-matcher "{%-? *raw *-?%}"
  :tail-matcher "{%-? *endraw *-?%}"
  :head-adjust-face '(:foreground "#ff88ff" :weight bold)
  :tail-adjust-face '(:foreground "#ff88ff" :weight bold)
  )

(define-innermode poly-jinja2-comment-innermode
  :mode nil                          ; inherit from host mode
  :head-matcher "{#-?"
  :tail-matcher "-?#}"
  :head-adjust-face 'font-lock-comment-delimiter-face
  :tail-adjust-face 'font-lock-comment-delimiter-face
  :adjust-face 'font-lock-comment-face
  :body-indent-offset 0
  :pre-indent-offset 0
  )

;; ======================================================
;; Poly Mode
;; ======================================================

(define-polymode poly-docker-jinja2-mode
  :hostmode 'poly-dockerfile-hostmode
  :innermodes '(poly-jinja2-comment-innermode poly-jinja2-raw-innermode poly-jinja2-innermode))   ; order-sensitive

(define-polymode poly-elisp-jinja2-mode
  :hostmode 'poly-elisp-hostmode
  :innermodes '(poly-jinja2-comment-innermode poly-jinja2-raw-innermode poly-jinja2-innermode))

(define-polymode poly-sh-jinja2-mode
  :hostmode 'poly-sh-hostmode
  :innermodes '(poly-jinja2-comment-innermode poly-jinja2-raw-innermode poly-jinja2-innermode))

(define-polymode poly-apparmor-jinja2-mode
  :hostmode 'poly-apparmor-hostmode
  :innermodes '(poly-jinja2-comment-innermode poly-jinja2-raw-innermode poly-jinja2-innermode))

;; ======================================================
;; auto-mode-alist
;; ======================================================

(dolist (entry '(("\\.\\(containerfile\\|dockerfile\\)\\.j2\\'" . poly-docker-jinja2-mode)
                 ("\\(Containerfile\\|Dockerfile\\)\\.j2\\'"    . poly-docker-jinja2-mode)
                 ("\\.el\\.j2\\'"                               . poly-elisp-jinja2-mode)
                 ("\\.zshrc\\.j2\\'"                            . poly-sh-jinja2-mode)
                 ("\\.bashrc\\.j2\\'"                           . poly-sh-jinja2-mode)
                 ("\\.aa\\.j2\\'"                               . poly-apparmor-jinja2-mode)
                 ))
  (add-to-list 'auto-mode-alist entry))

(provide 'rc-polymode)
