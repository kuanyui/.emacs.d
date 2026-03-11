;;; rc-polymode.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2026  ono

(require 'polymode)

(define-hostmode poly-dockerfile-hostmode
  :mode 'dockerfile-mode)

(define-innermode poly-jinja2-docker-innermode
  :mode 'jinja2-mode
  :head-matcher "{[{%]"
  :tail-matcher "[}%]}"
  :head-adjust-face '(:foreground "#ee88ff" :weight bold)
  :tail-adjust-face '(:foreground "#ee88ff" :weight bold)
  :fallback-mode 'host)

(define-polymode poly-docker-jinja2-mode
  :hostmode 'poly-dockerfile-hostmode
  :innermodes '(poly-jinja2-docker-innermode))

(add-to-list 'auto-mode-alist '("\\.\\(containerfile\\|dockerfile\\)\\.j2\\'" . poly-docker-jinja2-mode))
(add-to-list 'auto-mode-alist '("\\(Containerfile\\|Dockerfile\\)\\.j2\\'" . poly-docker-jinja2-mode))

(provide 'rc-polymode)
