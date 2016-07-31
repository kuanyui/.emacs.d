;;; qml-mode.el --- Major mode for editing QT Declarative (QML) code.

;; Copyright (C) 2010      Wen-Chun Lin
;; Copyright (C) 2013-2016 Yen-Chin Lee

;; Author: Yen-Chin Lee <coldnew.tw@gmail.com>
;; URL: https://github.com/coldnew/qml-mode
;; Package-Version: 20160108.704
;; Version: 0.3
;; Keywords: qml, qt, qt declarative

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;
;; qml-mode is major-mode for editing Qt Declarative (QML) code.
;;

;;; Installation:

;; If you have `melpa` and `emacs24` installed, simply type:
;;
;;      M-x package-install qml-mode
;;
;; Add following lines to your init file:
;;
;;      (autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
;;      (add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

;;; ChangeLog
;;
;; 0.3
;;
;;   * rewrite based on js-mode.
;;
;; 0.2
;;
;;   * rewrite based on generic-mode.
;;
;; 0.1
;;
;;   * first version fork from cataska/qml-mode.

;;; Code:
(require 'js)                           ; based on js-mode
(require 'font-lock)

;;; User Customization

(defgroup qml nil
  "Customization variables for QML mode."
  :tag "QML"
  :group 'languages)

;;; Functions

(defun qml--list-to-string (list)
  "Combine a list to string."
  (concat "\\(" (mapconcat 'identity list "\\|") "\\)"))

(defun qml-beginning-of-defun ()
  "Value of `beginning-of-defun-function' for `qml-mode'."
  (interactive)
  (re-search-backward "\{"))

(defun qml-end-of-defun ()
  "Value of `end-of-defun-function' for `qml-mode'."
  (interactive)
  (re-search-forward "\}"))

;;; KeyMap

(defvar qml-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-M-a") 'qml-beginning-of-defun)
    (define-key keymap (kbd "C-M-e") 'qml-end-of-defun)
    keymap)
  "Keymap for `qml-mode'.")

;;; Syntax table and parsing

(defconst qml--class-re
  (js--regexp-opt-symbol
   '("State" "PropertyChanges" "StateGroup" "ParentChange"
     "StateChangeScript" "AnchorChanges" "PropertyAnimation" "NumberAnimation"
     "ColorAnimation" "RotationAnimation" "SequentialAnimation" "ParallelAnimation"
     "PauseAnimation" "ParentAnimation" "AnchorAnimation" "SmoothedAnimation"
     "PropertyAction" "ScriptAction" "Transition" "SpringFollow"
     "Behavior" "Binding" "ListModel" "ListElement"
     "VisualItemModel" "VisualDataModel" "Package" "XmlListModel"
     "XmlRole" "Connections" "Component" "Timer"
     "QtObject" "WorkerScript" "Item" "Rectangle"
     "Image" "BorderImage" "Text" "TextInput"
     "TextEdit" "MouseArea" "FocusScope" "Flickable"
     "Flipable" "GestureArea" "Loader" "Repeater"
     "SystemPalette" "LayoutItem" "Scale" "Rotation"
     "Translate" "ViewsPositionersMediaEffects" "ListView" "GridView"
     "PathView" "Path" "PathLine" "PathQuad"
     "PathCubic" "PathAttribute" "PathPercent" "WebView"
     "Column" "Row" "Grid" "Flow"
     "SoundEffect" "Audio" "Video" "Particles"
     "ParticleMotionLinear" "ParticleMotionGravity" "ParticleMotionWander"))
  "Regexp matching any QML class")

(defconst qml--type-re
  (js--regexp-opt-symbol
   '("import" "signal" "Qt" "parent"))
  "Regular expression matching any predefined type in QML.")

(defconst qml--property
  '("bool" "double" "real" "int"
    "string" "url" "color" "date"
    "variant" "alias"))

(defconst qml--property-re
  (concat "\\(property[ \t]+" (qml--list-to-string qml--property) "\\)+[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"))

(defconst qml--constants-re
  (js--regexp-opt-symbol
   '("NoButton" "LeftButton" "RightButton" "MidButton"
     "MiddleButton"
     "Horizontal" "Vertical"
     "AlignLeft" "AlignRight" "AlignHCenter" "AlignTop"
     "AlignBottom" "AlignVCenter" "AlignCenter"
     "Easing" "Linear" "InQuad" "OutQuad"
     "InOutQuad" "OutInQuad" "InCubic" "OutCubic"
     "InOutCubic" "OutInCubic" "InQuart" "OutQuart"
     "InOutQuart" "OutInQuart" "InQuint" "InQuint"
     "OutQuint" "InOutQuint" "OutInQuint" "InSine"
     "OutSine" "InExpo" "OutExpo" "InOutExpo"
     "OutInExpo" "InCirc" "OutCirc" "InOutCirc"
     "OutInCirc" "InElastic" "OutElastic" "InOutElastic"
     "OutInElastic" "InBack" "OutBack" "InOutBack"
     "OutInBack" "InBounce" "OutBounce" "InOutBounce"
     "OutInBounce")))

(defconst qml--font-lock-keywords-1
  `( ;; Keywords
    (,qml--class-re (0 'font-lock-keyword-face))
    (,js--keyword-re (0 'font-lock-keyword-face))
    ;; Types
    (,qml--type-re (1 'font-lock-type-face))
    (,js--basic-type-re (1 'font-lock-type-face))
    ;; Constant
    (,qml--constants-re (0 font-lock-constant-face))
    (,js--constant-re (0 font-lock-constant-face))
    ("\\<id[ \t]*:[ \t]*\\([a-zA-Z0-9_]+\\)" (1 'font-lock-constant-face))
    ("\\([+-]?\\<[0-9]*\\.?[0-9]+[xX]?[0-9a-fA-F]*\\)" . font-lock-constant-face)
    ("\\([a-zA-Z_\\.]+[a-zA-Z0-9_]*\\)[ \t]*:" (1 font-lock-variable-name-face))
    ;; builtin
    ("\\([a-zA-Z0-9]+\\)[ \t]*{" (1 'font-lock-builtin-face))
    ;; Function
    ("\\(function\\|signal\\)\\{1\\}[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)" (2 'font-lock-function-name-face))
    ;; string
    ("\\('[[:alpha:]]*'\\)" (1 'font-lock-string-face)))
  "Keywords to highlight in `qml-mode'.")

(defconst qml--font-lock-keywords-2
  `(;; This goes before keywords-1 so it gets used preferentially
    ;; instead of the keywords in keywords-1. Don't use override
    ;; because that will override syntactic fontification too, which
    ;; will fontify commented-out directives as if they weren't
    ;; commented out.
    (,qml--property-re (1 'font-lock-type-face)
                       (3 'font-lock-variable-name-face))
    
    ,@qml--font-lock-keywords-1))

(defconst qml--font-lock-keywords
  '(qml--font-lock-keywords-2
    qml--font-lock-keywords-1
    qml--font-lock-keywords-2))

;;;###autoload
(define-derived-mode qml-mode js-mode "QML"
  "Major mode for editing QML.

\\{qml-mode-map}"
  :group 'qml

  (setq-local font-lock-defaults (list qml--font-lock-keywords))
  (setq-local beginning-of-defun-function 'qml-beginning-of-defun)
  (setq-local end-of-defun-function 'qml-end-of-defun)

  ;; C-style comment /* ... */
  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/[*/]+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\(?:\n\\|\\*+/\\)")
  (setq-local comment-continue " *"))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))


(provide 'qml-mode)

;;; qml-mode.el ends here
