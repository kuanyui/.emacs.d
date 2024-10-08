;;; early-init.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2023  ono

;; Author: ono <ono@ono-surfacego3>


;; https://stackoverflow.com/questions/13233285/howto-disable-the-emacs-site-start-files-permanently
(setq site-run-file nil)
(setq inhibit-default-init t)

(setq warning-minimum-level :emergency)

;; hide some UI elements in the early-init.el to gain some performance.
;; https://medium.com/@danielorihuelarodriguez/optimize-emacs-start-up-time-ae314201e04f
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq gc-cons-threshold (* 63 1000 1000)  ; default 8MB -> 63MB
      gc-cons-percentage 0.6)
