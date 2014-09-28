;;; qml-mode.el --- Major mode for editing QT Declarative (QML) code.

;; Copyright (C) 2013 Yen-Chin Lee

;; Author: Yen-Chin Lee <coldnew.tw@gmail.com>
;; URL: https://github.com/coldnew/qml-mode
;; Version: 20130427.808
;; X-Original-Version: 0.2
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
;; 	M-x package-install qml-mode
;;
;; Add following lines to your init file:
;;
;;	(autoload 'qml-mode "qml-mode" "Editing Qt Declarative." t)
;;	(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

;;; Code:

(require 'generic-x)
(require 'font-lock)

(defvar qml-mode-indent-offset 4
  "Indentation offset for `qml-mode'")

(defvar qml-mode-keywords
  '("Qt" "import" "property"
    "State" "PropertyChanges" "StateGroup" "ParentChange"
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
    "ParticleMotionLinear" "ParticleMotionGravity" "ParticleMotionWander"
    ;; javascript keywords
    "break"
    "case" "catch" "const" "continue"
    "debugger" "default" "delete" "do"
    "else" "enum"
    "false" "finally" "for" "function"
    "if" "in" "instanceof" "import"
    "let"
    "new" "null"
    "return"
    "switch"
    "this" "throw" "true" "false" "try" "typeof"
    "var" "void"
    "while" "with"
    "yield"
    "undefined"
    ))

(defvar qml-mode-types
  '("int" "bool" "double" "real"
    "string" "url" "color" "date"
    "variant" "alias"
    "signal" "on" "parent" "as"))

(defvar qml-mode-constants
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
    "OutInBounce"))


(defun qml-mode:list-to-string (list)
  ""
  (concat "\\("
          (mapconcat 'identity list "\\|")
          "\\)"))

;;;###autoload
(define-generic-mode qml-mode
  ;; comments
  '("//" ("/*" . "*/"))
  ;; keywords
  qml-mode-keywords
  ;; other fontlock
  (list
   (eval-when-compile
     (generic-make-keywords-list qml-mode-types 'font-lock-type-face))
   (eval-when-compile
     (generic-make-keywords-list qml-mode-constants 'font-lock-constant-face))
   (list "\\<id[ \t]*:[ \t]*\\([a-zA-Z0-9_]+\\)" 1 'font-lock-constant-face)
   (list
    (concat "property[ \t]+" (qml-mode:list-to-string qml-mode-types) "+[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)") 2 'font-lock-variable-name-face)
   (list "\\(function\\|signal\\)\\{1\\}[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)" 2 'font-lock-function-name-face)
   (list "\\([a-zA-Z_\\.]+[a-zA-Z0-9_]*\\)[ \t]*:" 1 'font-lock-type-face)
   (list "\\([+-]?\\<[0-9]*\\.?[0-9]+[xX]?[0-9a-fA-F]*\\)" 1 'font-lock-constant-face)
   (list "\\([a-zA-Z0-9]+\\)[ \t]*{" 1 'font-lock-builtin-face)
   (list "\\('[[:alpha:]]*'\\)" 1 'font-lock-string-face)
   )
  ;; filetype
  '("\\.qml$")

  ;; initializer
  (list
   (function
    (lambda ()
      (add-hook 'qml-mode-hook
                (lambda ()

                  ;; tab width
                  (set (make-local-variable 'tab-width) qml-mode-indent-offset)
                  (set (make-local-variable 'indent-tabs-mode) nil)
                  (set (make-local-variable 'indent-line-function) 'qml-mode-indent-line)
                  (set (make-local-variable 'indent-region-function) 'qml-mode-indent-region)

                  )
                nil 'local)
      ))))

(defun qml-in-comment-p ()
  "Check whether we are currently in a comment"
  (let ((here (point)))
    (and (search-backward "/*" nil t)
         (prog1
             (not (search-forward "*/" here t))
           (goto-char here) ))))


(defun qml-mode-indent-line ()
  "Indent the current line"
  (if (or (qml-in-comment-p)
          (looking-at "[ \t]*/\\*") )
      nil
    (save-excursion
      (let ((here (point))
            (depth 0))
        (while (and (forward-line -1)
                    (or (looking-at "^[ \t]*$")
                        (qml-in-comment-p) ))
          ;; Jump to a non comment/white-space line
          )
        (cond ((looking-at "\\([ \t]*\\)\\([^ \t].*\\)?{[ \t]*$")
               (setq depth (+ (- (match-end 1) (match-beginning 1))
                              qml-mode-indent-offset )))
              ((looking-at "\\([ \t]*\\)[^ \t]")
               (setq depth (- (match-end 1) (match-beginning 1))) )
              (t (setq depth 0)) )
        (goto-char here)
        (beginning-of-line)
        (if (looking-at "[ \t]*}")
            (setq depth (max (- depth qml-mode-indent-offset) 0)) )
        (if (looking-at "\\([ \t]*\\)")
            (if (= depth (- (match-end 1) (match-beginning 1)))
                nil
              (delete-region (match-beginning 1) (match-end 1))
              (indent-to depth))
          (if (> depth 0)
              (indent-to depth)))))
    (if (looking-at "[ \t]*")
        (end-of-line) )))


(defun qml-mode-indent-region (start end)
  (let ((indent-region-function nil))
    (indent-region start end nil)))


(provide 'qml-mode)

;;; qml-mode.el ends here
