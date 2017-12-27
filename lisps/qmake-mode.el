;;; qmake-mode-el -- Major mode for editing QMAKE files

;; Author: Hyungchan Kim <inlinechan@gmail.com>
;; Created: 10 Nov 2015
;; Keywords: QMAKE major-mode

;; Copyright (C) 2015 Hyungchan Kim <inlinechan@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Keywords and font-lock source from
;;   https://qmake-mode.googlecode.com/hg/qmake.el
;; ert from julia-mode.el
;;   https://github.com/JuliaLang/julia
;; I just implemented indentation in line-continuation and block

;; To run ERT(Emacs Lisp regression testing tool)
;; emacs -batch -l ert -l qmake-mode.el -f ert-run-tests-batch-and-exit
;; or
;; M-x ert <Enter> t <Enter>

;;; Code:

;; from sh-script.el
(defvar qmake-debug nil
  "Enable lots of debug messages - if function `qmake-debug' is enabled.")

;; Uncomment this defun and comment the defmacro for debugging.
;; (defun qmake-debug (&rest args)
;;   "For debugging:  display message ARGS if variable QMAKE-DEBUG is non-nil."
;;   (if qmake-debug
;;       (apply 'message args)))
(defmacro qmake-debug (&rest _args))

(defvar qmake-mode-hook nil)
(defvar qmake-mode-map
  (let ((qmake-mode-map (make-keymap)))
    (define-key qmake-mode-map "\C-j" 'newline-and-indent)
    qmake-mode-map)
  "Keymap for QMAKE major mode.")

(defcustom qmake-indent-width 4
  "The width for further indentation in qmake mode."
  :type 'integer
  :group 'qmake-mode)
(put 'qmake-indent-width 'safe-local-variable 'integerp)

(defvar qmake-functions-variables
  '("basename"
    "CONFIG"
    "contains"
    "count"
    "dirname"
    "error"
    "eval"
    "exists"
    "find"
    "for"
    "include"
    "infile"
    "isEmpty"
    "join"
    "member"
    "message"
    "prompt"
    "qoute"
    "sprintf"
    "system"
    "unique"
    "warning"
    "else"
    "unix"
    "win32"
    "mac")
  "Qmake function types."
  )

(defvar qmake-variables
  '("CONFIG"
    "DEFINES"
    "DEF_FILE"
    "DEPENDPATH"
    "DESTDIR"
    "DESTDIR_TARGET"
    "DLLDESTDIR"
    "DISTFILES"
    "DSP_TEMPLATE"
    "FORMS"
    "FORMS3"
    "HEADERS"
    "INCLUDEPATH"
    "INSTALLS"
    "LEXIMPLS"
    "LEXOBJECTS"
    "LEXSOURCES"
    "LIBS"
    "LITERAL_HASH"
    "MAKEFILE"
    "MAKEFILE_GENERATOR"
    "MOC_DIR"
    "OBJECTS"
    "OBJECTS_DIR"
    "OBJMOC"
    "POST_TARGETDEPS"
    "PRE_TARGETDEPS"
    "PRECOMPILED_HEADER"
    "QMAKE"
    "QMAKESPEC"
    "QMAKE_APP_FLAG"
    "QMAKE_APP_OR_DLL"
    "QMAKE_AR_CMD"
    "QMAKE_BUNDLE_DATA"
    "QMAKE_BUNDLE_EXTENSION"
    "QMAKE_CC"
    "QMAKE_CFLAGS_DEBUG"
    "QMAKE_CFLAGS_MT"
    "QMAKE_CFLAGS_MT_DBG"
    "QMAKE_CFLAGS_MT_DLL"
    "QMAKE_CFLAGS_MT_DLLDBG"
    "QMAKE_CFLAGS_RELEASE"
    "QMAKE_CFLAGS_SHLIB"
    "QMAKE_CFLAGS_THREAD"
    "QMAKE_CFLAGS_WARN_OFF"
    "QMAKE_CFLAGS_WARN_ON"
    "QMAKE_CLEAN"
    "QMAKE_CXX"
    "QMAKE_CXXFLAGS"
    "QMAKE_CXXFLAGS_DEBUG"
    "QMAKE_CXXFLAGS_MT"
    "QMAKE_CXXFLAGS_MT_DBG"
    "QMAKE_CXXFLAGS_MT_DLL"
    "QMAKE_CXXFLAGS_MT_DLLDBG"
    "QMAKE_CXXFLAGS_RELEASE"
    "QMAKE_CXXFLAGS_SHLIB"
    "QMAKE_CXXFLAGS_THREAD"
    "QMAKE_CXXFLAGS_WARN_OFF"
    "QMAKE_CXXFLAGS_WARN_ON"
    "QMAKE_EXTENSION_SHLIB"
    "QMAKE_EXT_MOC"
    "QMAKE_EXT_UI"
    "QMAKE_EXT_PRL"
    "QMAKE_EXT_LEX"
    "QMAKE_EXT_YACC"
    "QMAKE_EXT_OBJ"
    "QMAKE_EXT_CPP"
    "QMAKE_EXT_H"
    "QMAKE_FAILED_REQUIREMENTS"
    "QMAKE_FILETAGS"
    "QMAKE_FRAMEWORK_BUNDLE_NAME"
    "QMAKE_FRAMEWORK_VERSION"
    "QMAKE_INCDIR"
    "QMAKE_INCDIR_OPENGL"
    "QMAKE_INCDIR_QT"
    "QMAKE_INCDIR_THREAD"
    "QMAKE_INCDIR_X11"
    "QMAKE_LFLAGS"
    "QMAKE_LFLAGS_CONSOLE"
    "QMAKE_LFLAGS_CONSOLE_DLL"
    "QMAKE_LFLAGS_DEBUG"
    "QMAKE_LFLAGS_PLUGIN"
    "QMAKE_LFLAGS_QT_DLL"
    "QMAKE_LFLAGS_RELEASE"
    "QMAKE_LFLAGS_SHAPP"
    "QMAKE_LFLAGS_SHLIB"
    "QMAKE_LFLAGS_SONAME"
    "QMAKE_LFLAGS_THREAD"
    "QMAKE_LFLAGS_WINDOWS"
    "QMAKE_LFLAGS_WINDOWS_DLL"
    "QMAKE_LIBDIR"
    "QMAKE_LIBDIR_FLAGS"
    "QMAKE_LIBDIR_OPENGL"
    "QMAKE_LIBDIR_QT"
    "QMAKE_LIBDIR_X11"
    "QMAKE_LIBS"
    "QMAKE_LIBS_CONSOLE"
    "QMAKE_LIBS_OPENGL"
    "QMAKE_LIBS_OPENGL_QT"
    "QMAKE_LIBS_QT"
    "QMAKE_LIBS_QT_DLL"
    "QMAKE_LIBS_QT_OPENGL"
    "QMAKE_LIBS_QT_THREAD"
    "QMAKE_LIBS_RT"
    "QMAKE_LIBS_RTMT"
    "QMAKE_LIBS_THREAD"
    "QMAKE_LIBS_WINDOWS"
    "QMAKE_LIBS_X11"
    "QMAKE_LIBS_X11SM"
    "QMAKE_LIB_FLAG"
    "QMAKE_LINK_SHLIB_CMD"
    "QMAKE_POST_LINK"
    "QMAKE_PRE_LINK"
    "QMAKE_LN_SHLIB"
    "QMAKE_MAC_SDK"
    "QMAKE_MAKEFILE"
    "QMAKE_MOC_SRC"
    "QMAKE_QMAKE"
    "QMAKE_QT_DLL"
    "QMAKE_RESOURCE_FLAGS"
    "QMAKE_RUN_CC"
    "QMAKE_RUN_CC_IMP"
    "QMAKE_RUN_CXX"
    "QMAKE_RUN_CXX_IMP"
    "QMAKE_TARGET"
    "QMAKE_UIC"
    "QT"
    "QTPLUGIN"
    "RC_FILE"
    "RCC_DIR"
    "REQUIRES"
    "RES_FILE"
    "SOURCES"
    "SRCMOC"
    "SUBDIRS"
    "TARGET"
    "TARGET_EXT"
    "TARGET_x"
    "TARGET_x.y.z"
    "TEMPLATE"
    "TRANSLATIONS"
    "UICIMPLS"
    "UICOBJECTS"
    "UI_DIR"
    "UI_HEADERS_DIR"
    "UI_SOURCES_DIR"
    "VERSION"
    "VER_MAJ"
    "VER_MIN"
    "VER_PAT"
    "VPATH"
    "YACCIMPLS"
    "YACCOBJECTS"
    "YACCSOURCES")
  "Qmake variables."
  )

(defvar qmake-functions-regexp (regexp-opt qmake-functions-variables 'words))
(defvar qmake-variables-regexp (regexp-opt qmake-variables 'words))

(defvar qmake-font-lock-keywords
  (list
   '("#.*" . font-lock-comment-face)
   `(,qmake-functions-regexp . ,font-lock-function-name-face)
   `(,qmake-variables-regexp . ,font-lock-builtin-face))
  "Default highlighting expressions for QMAKE mode.")

(defvar qmake-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?# "< b" syntax-table)
    (modify-syntax-entry ?\n "> b" syntax-table)
    (modify-syntax-entry ?_ "w" syntax-table)
    syntax-table)
  "Syntax table for qmake-mode.")

(defun qmake-current-indent-type ()
  "Return current indent type."
  (let ((result))
    (save-excursion
      (end-of-line)
      (setq result (qmake-parse-line t)))
    (when (listp result)
      (car (car result))))
  )

(defun qmake-parent-indent-type ()
  "Return list of parent indent type.  Order by nearest first."
  (interactive)
  (let ((done)
        (line (save-excursion (forward-line -1)))
        (result))
    (save-excursion
      (while (and (not done) (= line 0))
        (setq line (forward-line -1))
        (end-of-line)
        (setq result (append result (qmake-parse-line nil)))))
    result))

(defun qmake-indent-line ()
  "Indent line when it's in `qmake-mode'."
  (interactive)
  (let* ((current (qmake-current-indent-type))
         (indent-base 0)
         (parent-list (qmake-parent-indent-type))
         (parent (and parent-list (car parent-list)))
         (parent-type (and parent (car parent)))
         (parent-point (and parent (cdr parent))))
    (let ((p)
          (pl parent-list)
          (pt parent-type)
          (pp))
      (when (eq pt 'line-cont-end)
        (while (or (eq pt 'line-cont-end) (eq pt 'line-cont))
          (setq p (car (cdr pl)))
          (setq pl (cdr pl))
          (setq pt (car p))
          (setq pp (cdr p)))
        (setq indent-base (save-excursion
                            (goto-char pp)
                            (current-indentation)))))
    (cond
     ((or (eq parent-type 'brace-open) (eq parent-type 'brace-close-open))
      (setq indent-base (save-excursion
                          (goto-char parent-point)
                          (+ (current-indentation) qmake-indent-width))))
     ((eq parent-type 'brace-close)
      (setq indent-base (save-excursion
                          (goto-char parent-point)
                          (current-indentation))))
     ((eq parent-type 'line-cont-begin)
      (setq indent-base (save-excursion
                          (goto-char parent-point)
                          (+ (current-indentation) qmake-indent-width))))
     ((eq parent-type 'line-cont)
      (setq indent-base (save-excursion
                          (goto-char parent-point)
                          (current-indentation)))))
    (qmake-debug "current: %s, parent: %s, indent-base: %s" current parent indent-base)
    (cond
     ((null parent)
      (indent-line-to (+ 0 indent-base)))
     ((eq current 'brace-close)
      (indent-line-to (save-excursion
                        (beginning-of-line)
                        (looking-at "^[ \t]*}")
                        (goto-char (match-end 0))
                        (ignore-errors
                          (backward-list 1))
                        (current-indentation))))
     ((eq current 'brace-close-open)
      (indent-line-to (save-excursion
                        (goto-char (cdr (assoc 'brace-open parent-list)))
                        (current-indentation))))
     ;; ((eq current 'default)
     ;;  (indent-line-to indent-base))
     ;; ((eq current 'line-cont-begin)
     ;;  (indent-line-to indent-base))
     ;; ((eq current 'brace-open)
     ;;  (indent-line-to indent-base))
     (t
      (indent-line-to indent-base))
     )))

 ;;;###autoload
(define-derived-mode qmake-mode prog-mode "qmake"
  "A major mode for qmake."
  :syntax-table qmake-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
              '(qmake-font-lock-keywords))
  (setq-local indent-line-function 'qmake-indent-line)
  (run-hooks 'qmake-mode-hook))

(defun qmake-parse-line (add-default)
  (let ((result))
    (cond
     ((looking-back "^.*[ \t]*\\\\$" (line-beginning-position))
      (progn
        (goto-char (match-end 0))
        (let ((prev-cont
               (save-excursion
                 (and (= (forward-line -1) 0)
                      (progn
                        (end-of-line)
                        (looking-back "^.*[ \t]*\\\\$"
                                      (line-beginning-position)))))))
          (if prev-cont
              (add-to-list 'result `(line-cont . ,(1- (point))))
            (add-to-list 'result `(line-cont-begin . ,(1- (point))))))))
     ((looking-back "^[ \t]*}.*[ \t]*{$" (line-beginning-position))
      (progn
        (goto-char (match-end 0))
        (add-to-list 'result `(brace-close-open . ,(1- (point))))))
     ((looking-back "^.*[ \t]*{$" (line-beginning-position))
      (progn
        (goto-char (match-end 0))
        (add-to-list 'result `(brace-open . ,(1- (point))))))
     ((looking-back "^[ \t]*}$" (line-beginning-position))
      (progn
        (goto-char (match-end 0))
        (add-to-list 'result `(brace-close . ,(1- (point))))))
     (t
      (let ((orig-point (point))
            (prev-line-cont
             (save-excursion
               (and (= 0 (forward-line -1))
                    (progn
                      (end-of-line)
                      (looking-back "^.*[ \t]*\\\\$"
                                    (line-beginning-position)))))))
        (if prev-line-cont
            (add-to-list 'result `(line-cont-end . ,(1- orig-point)))
          (when add-default
            (add-to-list 'result `(default . ,0))))))
     )                              ;cond
    result))

(add-to-list 'auto-mode-alist '("\\.pr\\(i\\|o\\|f\\)\\'" . qmake-mode))

(ignore-errors
  (require 'ert)

  ;; from julia-mode.el
  (defmacro qmake--should-indent (from to)
    "Assert that we indent text FROM producing text TO in `qmake-mode'."
    `(with-temp-buffer
       (qmake-mode)
       (setq-default c-basic-offset 4
                     indent-tabs-mode nil
                     default-tab-width 4
                     tab-width 4)
       (insert ,from)
       (indent-region (point-min) (point-max))
       (should (equal (buffer-substring-no-properties (point-min) (point-max))
                      ,to))))

  (ert-deftest qmake--test-indent-block ()
    "We should indent within block."
    (qmake--should-indent
     "
unix {
SOURCES += unix.cpp
}"
     "
unix {
    SOURCES += unix.cpp
}"))
  (ert-deftest qmake--test-line-cont ()
    "We should indent line continuation."
    (qmake--should-indent
     "
SOURCES += unix.cpp \\
linux.cpp
"
     "
SOURCES += unix.cpp \\
    linux.cpp
"))
  (ert-deftest qmake--test-line-cont-in-block ()
    "Test line continuation in block."
    (qmake--should-indent
     "
unix {
SOURCES += unix.cpp \\
linux.cpp
}
"
     "
unix {
    SOURCES += unix.cpp \\
        linux.cpp
}
"))
  (ert-deftest qmake--test-else-block ()
    "Else block should be indented accordingly."
    (qmake--should-indent
     "
!unix {
SOURCES += unix.cpp
} else {
SOURCES += other.cpp
}
"
     "
!unix {
    SOURCES += unix.cpp
} else {
    SOURCES += other.cpp
}
"))
  (ert-deftest qmake--test-multiple-line-cont ()
    "Test for multiple line continuation."
    (qmake--should-indent
     "
SOURCES += unix.cpp \\
linux.cpp \\
other.cpp
"
     "
SOURCES += unix.cpp \\
    linux.cpp \\
    other.cpp
"))
  (ert-deftest qmake--test-nesting-block ()
    "Test for nesting block."
    (qmake--should-indent
     "
config {
unix {
SOURCES += unix.cpp \\
other.cpp
}
}
"
     "
config {
    unix {
        SOURCES += unix.cpp \\
            other.cpp
    }
}
"))
  (ert-deftest qmake--test-nesting-block-in-depth ()
    "Test for nesting block."
    (qmake--should-indent
     "
    config {
    unix {
    SOURCES = unix.cpp \\
    other.cpp
    hello {
    SOURCES = world.cpp
    }
    }
    }
"
     "
config {
    unix {
        SOURCES = unix.cpp \\
            other.cpp
        hello {
            SOURCES = world.cpp
        }
    }
}
"))
  (ert-deftest qmake--test-block-after-line-cont ()
    "Test for block after line continuation."
    (qmake--should-indent
     "
    VPATH += \
    apple \
    tree

    contains(DEFINES, tomato) {
    VPATH += tomato
    } else {
    VPATH += potato
    }
"
     "
VPATH += \
    apple \
    tree

contains(DEFINES, tomato) {
    VPATH += tomato
} else {
    VPATH += potato
}
"))
  )


(provide 'qmake-mode)

;;; qmake-mode.el ends here
