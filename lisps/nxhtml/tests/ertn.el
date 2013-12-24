;;; ertn.el --- Emacs Lisp Regression Testing

;; Modified by Lennart Borgman 2008-07-13 to make all global symbols
;; use the "ert-" prefix.

;; 2011-11-02: Changed to use "ertn-" instead to avoid clashes with
;; the ert.el now in the package repository.

;; Copyright (C) 2007, 2008 Christian M. Ohler

;; Author: Christian M. Ohler
;; Version: 0.2
;; Keywords: lisp, tools

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; ERT is a tool for automated testing in Emacs Lisp.  Its main
;; features are facilities for defining and running test cases and
;; reporting the results as well as for debugging test failures
;; interactively.
;;
;; The main entry points are `ertn-deftest', which is similar to
;; `defun' but defines a test, and `ertn-run-tests-interactively',
;; which runs tests and offers an interactive interface for inspecting
;; results and debugging.  There is also `ertn-run-tests-batch' for
;; non-interactive use.
;;
;; The body of `ertn-deftest' forms resembles a function body, but the
;; additional operators `should', `should-not' and `should-error' are
;; available.  `should' is similar to cl's `assert', but signals a
;; different error when its condition is violated that is caught and
;; processed by ERT.  In addition, it analyzes its argument form and
;; records information that helps debugging (`assert' tries to do
;; something similar when its second argument SHOW-ARGS is true, but
;; `should' is more sophisticated).  For information on `should-not'
;; and `should-error', see their docstrings.
;;
;; For example,
;;
;;     ;; Define a test named `foo'.
;;     (ertn-deftest foo ()
;;       (ertn-should (= (+ 1 2) 4)))
;;
;;     ;; Run it.
;;     (ertn-run-tests-interactively 'foo)
;;
;; generates the following output (in addition to some statistics) in
;; the *ert* results buffer:
;;
;;     F foo
;;         (ertn-test-failed
;;          ((ertn-should
;;            (=
;;             (+ 1 2)
;;             4))
;;           :form
;;           (= 3 4)
;;           :value nil))
;;
;; This indicates that the test failed.  The `should' form that failed
;; was (ertn-should (= (+ 1 2) 4)), because its inner form, after
;; evaluation of its arguments, was the function call (= 3 4), which
;; returned nil.
;;
;; Obviously, this is a bug in the test case, not in the functions `+'
;; or `='.  In the results buffer, with point on the test result, the
;; key "." can be used to jump to the definition of the test to modify
;; it to correct the bug.  After evaluating the modified definition
;; and switching back to the results buffer, the key "r" will re-run
;; the test and show the new result.


;; Test selectors
;;
;; Functions like `ertn-run-tests-interactively' accept a test
;; selector, which is a Lisp expression specifying a set of tests.
;; Each test name is a selector that refers to that test, the selector
;; `t' refers to all tests, and the selector `:failed' refers to all
;; tests that failed; but more complex selectors are available.  Test
;; selector syntax is similar to cl's type specifier syntax.  See the
;; docstring of `ertn-select-tests' for details.


;; Comparison with other testing tools
;;
;; ERT allows test-driven development similar to *Unit frameworks for
;; other languages.  However, two common *Unit features are notably
;; absent from ERT: fixtures and test suites.
;;
;; Fixtures, as used e.g. in SUnit or JUnit, have two main purposes:
;; Setting up (and tearing down) an environment for a set of test
;; cases, and making that environment accessible through object
;; attributes that can be used like local variables.
;;
;; While fixtures are a great syntactic simplification in other
;; languages, they are not very useful in Lisp, where higher-order
;; functions and `unwind-protect' are available.  One way to implement
;; and use a fixture in ERT is
;;
;;    (defun my-fixture (body)
;;      (unwind-protect
;;          (progn ...set up...
;;                 (funcall body))
;;        ...tear down...))
;;
;;    (ertn-deftest my-test ()
;;      (my-fixture
;;       (lambda ()
;;         ...test code...)))
;;
;; (Another way would be a `with-my-fixture' macro.)  This solves the
;; set-up and tear-down part, and additionally allows any test case to
;; use any combination of fixtures, so it is more general than what
;; other tools typically allow.
;;
;; If the test case needs access to the environment the fixture sets
;; up, the fixture can be modified to pass arguments to the body.
;;
;; These are standard Lisp idioms.  Special syntax for them could be
;; added easily enough, but would provide only a minor simplification.
;;
;; (Note that splitting set-up and tear-down into separate functions,
;; like *Unit tools usually do, makes it impossible to establish
;; dynamic `let' bindings as part of the fixture.  So, blindly
;; imitating the way fixtures are implemented in other languages would
;; be counter-productive in Lisp.)
;;
;;
;; The purpose of test suites is to group related test cases together.
;; The most common use of this is to run just the tests for one
;; particular module.  Since symbol prefixes are the usual way of
;; separating module namespaces in Emacs Lisp, test selectors already
;; solve this by allowing regexp matching on test names; e.g., the
;; selector "^ertn-" selects ERT's self-tests.
;;
;; If test suites containing arbitrary sets of tests are found to be
;; desirable, it would be easy to add a `define-test-selector'
;; mechanism that introduces a new selector, defined in terms of
;; existing ones; e.g.
;;
;;     ;; Note that `define-test-selector' does not exist yet.
;;     (define-test-selector my-test-suite () `(member foo-test bar-test))
;;
;; would define a test suite named `my-test-suite' consisting of
;; `foo-test' and `bar-test'.  See also `deftype' in Common Lisp.


;; TODO: Add `skip' feature for tests that can't run in current environment.


;;; Code:

(eval-when-compile (require 'cl))
(require 'ewoc)
(require 'find-func)
(require 'debug)

(defvar ertn-debug-on-error nil
  "Non-nil means enter debugger when a test fails or terminates with an error.")


;;; Defining and locating tests.

;; The data structure that represents a test case.
(defstruct ertn-test
  (name nil)
  (documentation nil)
  (body (assert nil))
  (most-recent-result nil)
  (expected-result-type 'ertn-test-passed))

(defun ertn-test-boundp (symbol)
  "Return non-nil if SYMBOL names a test."
  (and (get symbol 'ertn-test) t))

(defun ertn-get-test (symbol)
  "If SYMBOL names a test, return that.  Signal an error otherwise."
  (assert (ertn-test-boundp symbol) t)
  (get symbol 'ertn-test))

(defun ertn-set-test (symbol doc definition)
  "Make SYMBOL name the test DEFINITION, and return DEFINITION."
  (when doc
    (put symbol 'ertn-test-documentation doc))
  (put symbol 'ertn-test definition)
  definition)

(defun ertn-make-test-unbound (symbol)
  "Make SYMBOL name no test.  Return SYMBOL."
  (remprop symbol 'ertn-test)
  symbol)

(defun ertn-test-result-expected-p (test result)
  "Return non-nil if RESULT matches the expected result type for TEST."
  (typep result (ertn-test-expected-result-type test)))

(defvar ertn-find-test-regexp
  (concat "^\\s-*(ertn-deftest"
          find-function-space-re
          "%s\\(\\s-\\|$\\)")
  "The regexp the `find-function' mechanisms use for locating test definitions.")

(eval-and-compile
  (defun ertn-parse-keys-and-body (docstr keys-and-body)
    "Split KEYS-AND-BODY into keyword-and-value pairs and the remaining body.

KEYS-AND-BODY should have the form of a property list, with the
exception that only keywords are permitted as keys and that the
tail -- the body -- is a list of forms that does not start with a
keyword.

Returns a two-element list containing the keys-and-values plist
and the body."
    (unless (stringp docstr)
      (when docstr
        (setq keys-and-body (cons docstr keys-and-body))
        (setq docstr nil)))
    (let ((extracted-key-accu '())
          (remaining keys-and-body))
      (while (and (consp remaining) (keywordp (first remaining)))
        (let ((keyword (pop remaining)))
          (unless (consp remaining)
            (error "Value expected after keyword %S in %S"
                   keyword keys-and-body))
          (when (assoc keyword extracted-key-accu)
            (warn "Keyword %S appears more than once in %S" keyword
                  keys-and-body))
          (push (cons keyword (pop remaining)) extracted-key-accu)))
      (setq extracted-key-accu (nreverse extracted-key-accu))
      (list (loop for (key . value) in extracted-key-accu
                  collect key
                  collect value)
            docstr
            remaining))))

(defvar ertn-error-on-test-redefinition nil)

;;;###autoload
(defmacro* ertn-deftest (name ()
                             &optional docstr
                             &body keys-and-body)
  "Define NAME (a symbol) as a test.

\(fn NAME () [:documentation DOCSTRING] [:expected-result TYPE] BODY...)"
  ;; The :documentation would be unreadable.  I have therefore added
  ;; docstr that will look like documentation use to in Emacs.  Maybe
  ;; add function ertn-describe-test?
  (declare (indent 2)
           (debug (&define :name test name sexp
                           [&optional [":documentation" stringp]]
                           [&optional [":expected-result" sexp]]
                           def-body)))
  (destructuring-bind ((&key (expected-result nil expected-result-supplied-p)
                             (documentation nil documentation-supplied-p))
                       doc
                       body)
      (ertn-parse-keys-and-body docstr keys-and-body)
    `(progn
       ;; Guard against missing/badly named tests:
       (when (and ertn-error-on-test-redefinition
                  (symbolp ',name)
                  (get ',name 'ertn-test))
         (with-output-to-temp-buffer "*Ert Error*"
           (with-current-buffer "*Ert Error*"
             (insert "Test "
                     (format "%s" ',name)
                     " is already defined in "
                     (format "%s" (find-definition-noselect ',name 'ertn-deftest))
                     "\n\n"
                     "Tip: Use `ertn-delete-all-tests' or `ertn-delete-test' before redefining tests."
                     )))
         (if (y-or-n-p "Do you want to call ertn-delete-all-tests and then continue? ")
             ;; Fix-me: This does not work, why?
             (ertn-delete-all-tests)
           (error "Test %s is already defined in %s"
                  ',name
                  (find-definition-noselect ',name 'ertn-deftest))))
       (ertn-set-test ',name
                     nil ;;doc
                     (make-ertn-test
                      :name ',name
                      :body (lambda () ,@body)
                      ,@(when expected-result-supplied-p
                          `(:expected-result-type ,expected-result))
                      ,@(when documentation-supplied-p
                          `(:documentation ,documentation))))
       ;; This hack allows `symbol-file' to associate `ertn-deftest'
       ;; forms with files, and therefore enables `find-function' to
       ;; work with tests.  However, it leads to warnings in
       ;; `unload-feature', which doesn't know how to undefine tests
       ;; and has no mechanism for extension.
       (push '(ertn-deftest . ,name) current-load-list)
       ',name)))

(defun ertn-read-test-name (prompt &optional default-value history)
  "Read the name of a test and return it as a symbol.
Prompt with PROMPT.  By default, return DEFAULT-VALUE."
  (when (symbolp default-value) (setq default-value (symbol-name default-value)))
  (intern (completing-read prompt obarray #'ertn-test-boundp
                           t nil history default-value nil)))

(defun ertn-find-test-other-window (test-name)
  "Find, in another window, the definition of TEST-NAME."
  (interactive (list (ertn-read-test-name "Find test definition: ")))
  (find-function-do-it test-name 'ertn-deftest 'switch-to-buffer-other-window))

(defun ertn-delete-test (test-name)
  "An interactive interface to `ertn-make-test-unbound'."
  (interactive (list (let ((default (thing-at-point 'symbol)))
                       (when default
                         (set-text-properties 0 (length default) nil default)
                         (when (or (string= default "nil") (intern-soft default))
                           (setq default (intern default)))
                         (unless (ertn-test-boundp default)
                           (setq default nil)))
                       (completing-read (if (null default)
                                            "Delete test: "
                                          (format "Delete test (default %s): "
                                                  default))
                                        obarray #'ertn-test-boundp
                                        'really-require-match
                                        nil nil default nil))))
  (ertn-make-test-unbound test-name))

(defun ertn-delete-all-tests ()
  "Make all symbols in `obarray' name no test."
  (interactive)
  (when (interactive-p)
    (unless (y-or-n-p "Delete all tests? ")
      (error "Aborted")))
  (mapc #'ertn-delete-test (mapcar #'ertn-test-name (ertn-select-tests t t)))
  t)


(defun ertn-make-end-marker (buffer must-exist)
  "Return a marker to the end of buffer BUFFER.
BUFFER may be a string or a buffer. If BUFFER does not exist
return nil.

The buffer must exist if MUST-EXIST is non-nil.

See also:
 `ertn-end-of-messages'
 `ertn-end-of-warnings'"
  (let ((buf (if must-exist
                 (get-buffer buffer)
               (get-buffer-create buffer))))
    (when (and buf
               (bufferp buf)
               (buffer-live-p buf))
      (with-current-buffer buf
        (save-restriction
          (widen)
          (point-max-marker))))))

(defun ertn-end-of-messages ()
  "Return a marker to the end of *Messages* buffer."
  (ertn-make-end-marker "*Messages*" nil))

(defun ertn-end-of-warnings ()
  "Return a marker to the end of *Warnings* buffer."
  (ertn-make-end-marker "*Warnings*" nil))

(defun ertn-search-after (after regexp)
  "Search after marker in AFTER for regular expression REGEXP.
Return a alist of position and matches.  AFTER should have been
created with `ertn-make-end-marker'.

This is supposed to be used for messages and trace buffers.

See also
 `ertn-get-messages'"
  (let ((buf (marker-buffer after)))
    (with-current-buffer buf
      (let ((here (point))
            res)
        (goto-char after)
        (save-match-data
          (while (re-search-forward regexp nil t)
            (setq res (cons (match-data) res))))
        (goto-char here)
        (reverse res)))))
;; fix-me: add a conventient way to look at the result of
;; `ertn-search-after'. Probably this means adding something more to
;; the returned result.

(defvar ertn-messages-mark)
(defun ertn-get-messages (regexp)
  "Search *Messages* buffer for regular expression REGEXP.
This should be used within `ertn-deftest'.  Search begins where
the buffer ended when test started.

See also:
 `ertn-get-warnings'
 `ertn-search-after'"
  (ertn-search-after ertn-messages-mark regexp))

(defvar ertn-warnings-mark)
(defun ertn-get-warnings (regexp)
  "Search *Warnings* buffer for regular expression REGEXP.
See `ertn-get-messages' for more information."
  (ertn-search-after ertn-warnings-mark regexp))


;;; Test selectors.

(defun ertn-select-tests (selector universe)
  "Select, from UNIVERSE, a set of tests according to SELECTOR.

UNIVERSE should be a list of tests, or t, which refers to all
tests named by symbols in `obarray'.

Returns the set of tests as a list.

Valid selectors:

nil -- Selects the empty set.
t -- Selects UNIVERSE.
:new -- Selects all tests that have not been run yet.
:failed, :passed, :error -- Select tests according to their most recent result.
:expected, :unexpected -- Select tests according to their most recent result.
a string -- Selects all tests that have a name that matches the string, a regexp.
a test -- Selects that test.
a symbol -- Selects the test that the symbol names, errors if none.
\(member TESTS...\) -- Selects TESTS, a list of tests or symbols naming tests.
\(eql TEST\) -- Selects TEST, a test or a symbol naming a test.
\(and SELECTORS...\) -- Selects the tests that match all SELECTORS.
\(or SELECTORS...\) -- Selects the tests that match any SELECTOR.
\(not SELECTOR\) -- Selects all tests that do not match SELECTOR.
\(satisfies PREDICATE\) -- Selects all tests that satisfy PREDICATE.

Only selectors that require a superset of tests, such
as (satisfies ...), strings, :new, etc. make use of UNIVERSE.
Selectors that do not, such as \(member ...\), just return the
set implied by them without checking whether it is really
contained in UNIVERSE."
  ;; This code needs to match the etypecase in
  ;; `ertn-insert-human-readable-selector'.
  (etypecase selector
    ((member nil) nil)
    ((member t) (etypecase universe
                  (list universe)
                  ((member t) (ertn-select-tests "" universe))))
    ((member :new) (ertn-select-tests
                    `(satisfies ,(lambda (test)
                                   (typep (ertn-test-most-recent-result test)
                                          'null)))
                    universe))
    ((member :failed) (ertn-select-tests
                       `(satisfies ,(lambda (test)
                                      (typep (ertn-test-most-recent-result test)
                                             'ertn-test-failed)))
                       universe))
    ((member :passed) (ertn-select-tests
                       `(satisfies ,(lambda (test)
                                      (typep (ertn-test-most-recent-result test)
                                             'ertn-test-passed)))
                       universe))
    ((member :error) (ertn-select-tests
                      `(satisfies ,(lambda (test)
                                     (typep (ertn-test-most-recent-result test)
                                            'ertn-test-error)))
                      universe))
    ((member :expected) (ertn-select-tests
                         `(satisfies
                           ,(lambda (test)
                              (ertn-test-result-expected-p
                               test
                               (ertn-test-most-recent-result test))))
                         universe))
    ((member :unexpected) (ertn-select-tests `(not :expected) universe))
    (string
     (etypecase universe
       ((member t) (mapcar #'ertn-get-test
                           (apropos-internal selector #'ertn-test-boundp)))
       (list (remove-if-not (lambda (test)
                              (and (ertn-test-name test)
                                   (string-match selector (ertn-test-name test))))
                            universe))))
    (ertn-test (list selector))
    (symbol
     (assert (ertn-test-boundp selector))
     (list (ertn-get-test selector)))
    (cons
     (destructuring-bind (operator &rest operands) selector
       (ecase operator
         (member
          (mapcar (lambda (purported-test)
                    (etypecase purported-test
                      (symbol (assert (ertn-test-boundp purported-test))
                              (ertn-get-test purported-test))
                      (ertn-test purported-test)))
                  operands))
         (eql
          (assert (eql (length operands) 1))
          (ertn-select-tests `(member ,@operands) universe))
         (and
          ;; Do these definitions of AND, NOT and OR satisfy de
          ;; Morgan's rules?  Should they?
          (case (length operands)
            (0 (ertn-select-tests 't universe))
            (t (ertn-select-tests `(and ,@(rest operands))
                                 (ertn-select-tests (first operands) universe)))))
         (not
          (assert (eql (length operands) 1))
          (set-difference (ertn-select-tests 't universe)
                          (ertn-select-tests (first operands) universe)))
         (or
          (case (length operands)
            (0 (ertn-select-tests 'nil universe))
            (t (union (ertn-select-tests (first operands) universe)
                      (ertn-select-tests `(or ,@(rest operands)) universe)))))
         (satisfies
          (assert (eql (length operands) 1))
          (remove-if-not (first operands) (ertn-select-tests 't universe))))))))

(defun ertn-insert-human-readable-selector (selector)
  "Insert a human-readable presentation of SELECTOR into the current buffer."
  ;; This is needed to avoid printing the (huge) contents of the
  ;; `backtrace' slot of the result objects in the
  ;; `most-recent-result' slots of test case objects in (eql ...) or
  ;; (member ...) selectors.
  (labels ((rec (selector)
             ;; This code needs to match the etypecase in `ertn-select-tests'.
             (etypecase selector
               ((or (member nil t
                            :new :failed :passed :error
                            :expected :unexpected)
                    string
                    symbol)
                selector)
               (ertn-test
                (if (ertn-test-name selector)
                    (make-symbol (format "<%S>" (ertn-test-name selector)))
                  (make-symbol "<unnamed test>")))
               (cons
                (destructuring-bind (operator &rest operands) selector
                  (ecase operator
                    ((member eql and not or)
                     `(,operator ,@(mapcar #'rec operands)))
                    (satisfies
                     selector)))))))
    (insert (format "%S" (rec selector)))))


;;; Running tests.

(put 'ertn-test-failed 'error-conditions '(error ertn-test-failed))
(put 'ertn-test-failed 'error-message "Test failed")

(defun ertn-pass ()
  "Terminate the current test and mark it passed.  Does not return."
  (throw 'ertn-pass nil))

(defun ertn-fail (data)
  "Terminate the current test and mark it failed.  Does not return.
DATA is displayed to the user and should state the reason of the failure."
  (signal 'ertn-test-failed (list data)))

;; The data structures that represent the result of running a test.
(defstruct ertn-test-result
  (messages nil)
  )
(defstruct (ertn-test-passed (:include ertn-test-result)))
(defstruct (ertn-test-result-with-condition (:include ertn-test-result))
  (condition (assert nil))
  (backtrace (assert nil)))
(defstruct (ertn-test-error (:include ertn-test-result-with-condition)))
(defstruct (ertn-test-quit (:include ertn-test-result-with-condition)))
(defstruct (ertn-test-failed (:include ertn-test-result-with-condition)))
(defstruct (ertn-test-aborted-with-non-local-exit (:include ertn-test-result)))


(defun ertn-record-backtrace ()
  "Record the current backtrace (as a list) and return it."
  ;; Since the backtrace is stored in the result object, result
  ;; objects must only be printed with appropriate limits
  ;; (`print-level' and `print-length') in place.  For interactive
  ;; use, the cost of ensuring this possibly outweighs the advantage
  ;; of storing the backtrace for
  ;; `ertn-results-pop-to-backtrace-for-test-at-point' given that we
  ;; already have `ertn-results-rerun-test-debugging-errors-at-point'.
  ;; For batch use, however, printing the backtrace may be useful.
  (loop
   ;; 6 is the number of frames our own debugger adds (when
   ;; compiled; more when interpreted).  FIXME: Need to describe a
   ;; procedure for determining this constant.
   for i from 6
   for frame = (backtrace-frame i)
   while frame
   collect frame))

;; A container for the state of the execution of a single test and
;; environment data needed during its execution.
(defstruct ertn-test-execution-info
  (test (assert nil))
  (result (assert nil))
  ;; A thunk that may be called when RESULT has been set to its final
  ;; value and test execution should be terminated.  Should not
  ;; return.
  (exit-continuation (assert nil))
  ;; The binding of `debugger' outside of the execution of the test.
  next-debugger
  ;; The binding of `ertn-debug-on-error' that is in effect for the
  ;; execution of the current test.  We store it to avoid being
  ;; affected by any new bindings the test itself may establish.  (I
  ;; don't remember whether this feature is important.)
  ertn-debug-on-error)

(defun ertn-run-test-debugger (info debugger-args)
  "The function that `debugger' is bound to during the execution of tests.

Records failures and errors and either terminates the test
silently or calls the interactive debugger, as appropriate."
  (destructuring-bind (first-debugger-arg &rest more-debugger-args) debugger-args
    (ecase first-debugger-arg
      ((lambda debug t exit nil)
       (apply (ertn-test-execution-info-next-debugger info) debugger-args))
      (error
       (let* ((condition (first more-debugger-args))
              (type (case (car condition)
                      ((quit) 'quit)
                      ((ertn-test-failed) 'failed)
                      (otherwise 'error)))
              (backtrace (ertn-record-backtrace)))
         (setf (ertn-test-execution-info-result info)
               (ecase type
                 (quit
                  (make-ertn-test-quit :condition condition
                                      :backtrace backtrace))
                 (failed
                  (make-ertn-test-failed :condition condition
                                        :backtrace backtrace))
                 (error
                  (make-ertn-test-error :condition condition
                                       :backtrace backtrace))))
         ;; Work around Emacs' heuristic (in eval.c) for detecting
         ;; errors in the debugger.
         (incf num-nonmacro-input-events)
         ;; FIXME: We should probably implement more fine-grained
         ;; control a la non-t `debug-on-error' here.
         (cond
          ((ertn-test-execution-info-ertn-debug-on-error info)
           (apply (ertn-test-execution-info-next-debugger info) debugger-args))
          (t))
         (funcall (ertn-test-execution-info-exit-continuation info)))))))

(defun ertn-run-test-internal (ertn-test-execution-info)
  (lexical-let ((info ertn-test-execution-info))
    (setf (ertn-test-execution-info-next-debugger info) debugger
          (ertn-test-execution-info-ertn-debug-on-error info) ertn-debug-on-error)
    (catch 'ertn-pass
      ;; For now, each test gets its own temp buffer and its own
      ;; window excursion, just to be safe.  If this turns out to be
      ;; too expensive, we can remove it.
      (with-temp-buffer
        (save-window-excursion
          (let ((debugger (lambda (&rest debugger-args)
                            (ertn-run-test-debugger info debugger-args)))
                (debug-on-error t)
                (debug-on-quit t)
                ;; FIXME: Do we need to store the old binding of this
                ;; and consider it in `ertn-run-test-debugger'?
                (debug-ignored-errors nil)
                (ertn-messages-mark (ertn-end-of-messages))
                (ertn-warnings-mark (ertn-end-of-warnings)))
            (funcall (ertn-test-body (ertn-test-execution-info-test info))))))
      (ertn-pass))
    (setf (ertn-test-execution-info-result info) (make-ertn-test-passed)))
  nil)

(defun ertn-make-marker-in-messages-buffer ()
  (with-current-buffer (get-buffer-create "*Messages*")
    (set-marker (make-marker) (point-max))))

(defun ertn-force-message-log-buffer-truncation ()
  (with-current-buffer (get-buffer-create "*Messages*")
    ;; This is a reimplementation of this part of message_dolog() in xdisp.c:
    ;; if (NATNUMP (Vmessage_log_max))
    ;;   {
    ;;     scan_newline (Z, Z_BYTE, BEG, BEG_BYTE,
    ;;                   -XFASTINT (Vmessage_log_max) - 1, 0);
    ;;     del_range_both (BEG, BEG_BYTE, PT, PT_BYTE, 0);
    ;;   }
    (when (and (integerp message-log-max) (>= message-log-max 0))
      (let ((begin (point-min))
            (end (save-excursion
                   (goto-char (point-max))
                   (forward-line (- message-log-max))
                   (point))))
        (delete-region begin end)))))

(defun ertn-run-test (test)
  "Run TEST.  Return the result and store it in TEST's `most-recent-result' slot."
  (setf (ertn-test-most-recent-result test) nil)
  (block error
    (lexical-let* ((begin-marker (ertn-make-marker-in-messages-buffer))
                   (info (make-ertn-test-execution-info
                         :test test
                         :result (make-ertn-test-aborted-with-non-local-exit)
                         :exit-continuation (lambda ()
                                              (return-from error nil)))))
      (unwind-protect
          (let ((message-log-max t))
            (ertn-run-test-internal info))
        (let ((result (ertn-test-execution-info-result info)))
          (setf (ertn-test-result-messages result)
                (with-current-buffer (get-buffer-create "*Messages*")
                  (buffer-substring begin-marker (point-max))))
          (ertn-force-message-log-buffer-truncation)
          (setf (ertn-test-most-recent-result test) result)))))
  (ertn-test-most-recent-result test))


;;; The `should' macros.

(eval-and-compile
  (defun ertn-special-operator-p (thing)
    "Return non-nil if THING is a symbol naming a special operator."
    (and (symbolp thing)
         (let ((definition (indirect-function thing t)))
           (and (subrp definition)
                (eql (cdr (subr-arity definition)) 'unevalled)))))
  (defun ertn-expand-should (whole form env inner-expander)
    "Helper function for the `should' macro and its variants.

Analyzes FORM and produces an expression that has the same
semantics under evaluation but records additional debugging
information.  INNER-EXPANDER adds the actual checks specific to
the particular variant of `should'."
    (let ((form (macroexpand form env)))
      ;; It's sort of a wart that `inner-expander' can't influence the
      ;; value the expansion returns.
      (cond
       ((atom form)
        (funcall inner-expander form `(list ',whole :form ',form :value ,form)))
       ((ertn-special-operator-p (car form))
        (let ((value (gensym "value-")))
          `(let ((,value (make-symbol "ertn-form-evaluation-aborted")))
             ,(funcall inner-expander
                       `(setq ,value ,form)
                       `(list ',whole :form ',form :value ,value))
             ,value)))
       (t
        (let ((fn-name (car form))
              (arg-forms (cdr form)))
          (assert (or (symbolp fn-name)
                      (and (consp fn-name)
                           (eql (car fn-name) 'lambda)
                           (listp (cdr fn-name)))))
          (let ((fn (gensym "fn-"))
                (args (gensym "args-"))
                (value (gensym "value-"))
                (default-value (gensym "ertn-form-evaluation-aborted-")))
            `(let ((,fn (function ,fn-name))
                   (,args (list ,@arg-forms)))
               (let ((,value ',default-value))
                 ,(funcall inner-expander
                           `(setq ,value (apply ,fn ,args))
                           `(nconc (list ',whole)
                                   (list :form `(,,fn ,@,args))
                                   (unless (eql ,value ',default-value)
                                     (list :value ,value))
                                   (let ((-explainer-
                                          (and (symbolp ',fn-name)
                                               (get ',fn-name
                                                    'ertn-explainer))))
                                     (when -explainer-
                                       (list :explanation
                                             (apply -explainer- ,args))))))
                 ,value)))))))))

(defmacro* ertn-should (form &environment env)
  "Evaluate FORM.  If it returns nil, abort the current test as failed.

Returns the value of FORM."
  (ertn-expand-should `(ertn-should ,form) form env
                     (lambda (inner-form form-description-form)
                       `(unless ,inner-form
                          (ertn-fail ,form-description-form)))))

(defmacro* ertn-should-not (form &environment env)
  "Evaluate FORM.  If it returns non-nil, abort the current test as failed.

Returns nil."
  (ertn-expand-should `(ertn-should-not ,form) form env
                     (lambda (inner-form form-description-form)
                       `(unless (not ,inner-form)
                          (ertn-fail ,form-description-form)))))

(defun ertn-should-error-handle-error (form-description-fn
                                      condition type exclude-subtypes test)
  "Helper function for `should-error'.

Determines whether CONDITION matches TYPE, EXCLUDE-SUBTYPES and
TEST, and aborts the current test as failed if it doesn't."
  (let ((signalled-conditions (get (car condition) 'error-conditions))
        (handled-conditions (etypecase type
                              (list type)
                              (symbol (list type)))))
    (assert signalled-conditions)
    (unless (intersection signalled-conditions handled-conditions)
      (ertn-fail (append
                 (funcall form-description-fn)
                 (list
                  :condition condition
                  :fail-reason (concat "the error signalled did not"
                                       " have the expected type")))))
    (when exclude-subtypes
      (unless (member (car condition) handled-conditions)
        (ertn-fail (append
                   (funcall form-description-fn)
                   (list
                    :condition condition
                    :fail-reason (concat "the error signalled was a subtype"
                                         " of the expected type"))))))
    (unless (funcall test condition)
      (ertn-fail (append
                 (funcall form-description-fn)
                 (list
                  :condition condition
                  :fail-reason "the error signalled did not pass the test"))))))

;; FIXME: The expansion will evaluate the keyword args (if any) in
;; nonstandard order.
(defmacro* ertn-should-error (form &rest keys &key type exclude-subtypes test
                              &environment env)
  "Evaluate FORM.  Unless it signals an error, abort the current test as failed.

The error signalled additionally needs to match TYPE and satisfy
TEST.  TYPE should be a condition name or a list of condition
names.  If EXCLUDE-SUBTYPES is nil, the error matches TYPE if one
of its condition names is an element of TYPE.  If
EXCLUDE-SUBTYPES is non-nil, the error matches TYPE if it is an
element of TYPE.  TEST should be a predicate."
  ;; Returns a gensym named `ertn-form-evaluation-aborted-XXX', but
  ;; that's a wart, so let's not document it.
  (unless type (setq type ''error))
  (unless test (setq test '(lambda (condition) t)))
  (ertn-expand-should
   `(ertn-should-error ,form ,@keys)
   form env
   (lambda (inner-form form-description-form)
     (let ((errorp (gensym "errorp"))
           (form-description-fn (gensym "form-description-fn-")))
       `(let ((,errorp nil)
              (,form-description-fn (lambda () ,form-description-form)))
          (condition-case -condition-
              ,inner-form
            ;; We can't use ,type here because we want to evaluate it.
            (error
             (setq ,errorp t)
             (ertn-should-error-handle-error ,form-description-fn
                                            -condition-
                                            ,type ,exclude-subtypes ,test)
             ;; It would make sense to have the `should-error' form
             ;; return the error in this case, but `ertn-expand-should'
             ;; doesn't allow that at the moment.
             ))
          (unless ,errorp
            (ertn-fail (append
                       (funcall ,form-description-fn)
                       (list
                        :fail-reason "did not signal an error")))))))))


;;; Explanation of `should' failures.

(defun ertn-proper-list-p (x)
  "Return non-nil if X is a proper list, nil otherwise."
  (loop
   for firstp = t then nil
   for fast = x then (cddr fast)
   for slow = x then (cdr slow) do
   (when (null fast) (return t))
   (when (not (consp fast)) (return nil))
   (when (null (cdr fast)) (return t))
   (when (not (consp (cdr fast))) (return nil))
   (when (and (not firstp) (eq fast slow)) (return nil))))

(defun ertn-explain-not-equal (a b)
  "Return a programmer-readable explanation of why A and B are not `equal'.

Returns nil if they are equal."
  (if (not (equal (type-of a) (type-of b)))
      `(different-types ,a ,b)
    (etypecase a
      (cons
       (let ((a-proper-p (ertn-proper-list-p a))
             (b-proper-p (ertn-proper-list-p b)))
         (if (not (eql (not a-proper-p) (not b-proper-p)))
             `(one-list-proper-one-improper ,a ,b)
           (if a-proper-p
               (if (not (equal (length a) (length b)))
                   ;; This would be even more helpful if it showed
                   ;; something like what `set-difference' would
                   ;; return.
                   `(proper-lists-of-different-length ,a ,b)
                 (loop for i from 0
                       for ai in a
                       for bi in b
                       for xi = (ertn-explain-not-equal ai bi)
                       do (when xi (return `(list-elt ,i ,xi)))))
             (let ((car-x (ertn-explain-not-equal (car a) (car b))))
               (if car-x
                   `(car ,car-x)
                 (let ((cdr-x (ertn-explain-not-equal (cdr a) (cdr b))))
                   (if cdr-x
                       `(cdr ,cdr-x))
                   nil)))))))
      (array (if (not (equal (length a) (length b)))
                 `(arrays-of-different-length ,a ,b)
               (loop for i from 0
                     for ai across a
                     for bi across b
                     for xi = (ertn-explain-not-equal ai bi)
                     do (when xi (return `(array-elt ,i ,xi))))))
      (atom (if (not (equal a b))
                `(different-atoms ,a ,b)
              nil)))))
(put 'equal 'ertn-explainer 'ertn-explain-not-equal)


;;; Results display.

;; The data structure that contains the set of tests being executed
;; during one particular test run, their results, the state of the
;; execution, and some statistics.
;;
;; The data about results and expected results of tests may seem
;; redundant here, since the test objects also carry such information.
;; However, the information in the test objects may be more recent, it
;; may correspond to a different test run.  We need the information
;; that corresponds to this run in order to be able to update the
;; statistics correctly when a test is re-run interactively and has a
;; different result than before.
(defstruct ertn-stats
  (selector (assert nil))
  ;; The tests, in order.
  (tests (assert nil) :type vector)
  ;; A map of test names (or the test objects themselves for unnamed
  ;; tests) to indices into the `tests' vector.
  (test-map (assert nil) :type hash-table)
  ;; The results of the tests during this run, in order.
  (test-results (assert nil) :type vector)
  ;; The expected result types of the tests, in order.
  (test-results-expected (assert nil) :type vector)
  (total (assert nil))
  (passed-expected 0)
  (passed-unexpected 0)
  (failed-expected 0)
  (failed-unexpected 0)
  (error-expected 0)
  (error-unexpected 0)
  (start-time (assert nil))
  (end-time nil)
  (aborted-p nil)
  (current-test nil))

;; An entry in the results buffer ewoc.  There is one entry per test.
(defstruct ertn-ewoc-entry
  (test (assert nil))
  (result nil)
  ;; If the result of this test was expected, its ewoc entry is hidden
  ;; initially.
  (hidden-p (assert nil))
  ;; An ewoc entry may be collapsed to hide details such as the error
  ;; condition.
  ;;
  ;; I'm not sure the ability to expand and collapse entries is still
  ;; a useful feature.
  (expanded-p t)
  ;; By default, the ewoc entry presents the error condition with
  ;; certain limits on how much to print (`print-level',
  ;; `print-length').  The user can interactively switch to a set of
  ;; higher limits.
  (extended-printer-limits-p nil))

;; Variables local to the results buffer.

;; The ewoc.
(defvar ertn-results-ewoc)
;; The stats object.
(defvar ertn-results-stats)
;; A string with one character per test.  Each character represents
;; the result of the corresponding test.  The string is displayed near
;; the top of the buffer and serves as a progress bar.
(defvar ertn-results-progress-bar-string)
;; The position where the progress bar button begins.
(defvar ertn-results-progress-bar-button-begin)
;; The test result listener that updates the buffer when tests are run.
(defvar ertn-results-listener)

;; The same as `ertn-results-stats', but dynamically bound.  Used for
;; the mode line progress indicator.
(defvar ertn-current-run-stats nil)

(defun ertn-format-time-iso8601 (time)
  "Format TIME in the particular variant of ISO 8601 used for timestamps in ERT."
  (format-time-string "%Y-%m-%d %T%z" time))

(defun ertn-insert-test-name-button (test-name)
  (insert-text-button (format "%S" test-name)
                      :type 'ertn-test-name-button
                      'ertn-test-name test-name))

(defun ertn-results-update-ewoc-hf (ewoc stats)
  "Update the header and footer of EWOC to show certain information from STATS.

Also sets `ertn-results-progress-bar-button-begin'."
  (let ((run-count (+ (ertn-stats-passed-expected stats)
                      (ertn-stats-passed-unexpected stats)
                      (ertn-stats-failed-expected stats)
                      (ertn-stats-failed-unexpected stats)
                      (ertn-stats-error-expected stats)
                      (ertn-stats-error-unexpected stats)))
        (results-buffer (current-buffer)))
    (ewoc-set-hf
     ewoc
     ;; header
     (with-temp-buffer
       (insert "Selector: ")
       (ertn-insert-human-readable-selector (ertn-stats-selector stats))
       (insert "\n")
       (insert
        (format (concat "Passed: %s (%s unexpected)\n"
                        "Failed: %s (%s unexpected)\n"
                        "Error:  %s (%s unexpected)\n"
                        "Total:  %s/%s\n\n")
                (+ (ertn-stats-passed-expected stats)
                   (ertn-stats-passed-unexpected stats))
                (ertn-stats-passed-unexpected stats)
                (+ (ertn-stats-failed-expected stats)
                   (ertn-stats-failed-unexpected stats))
                (ertn-stats-failed-unexpected stats)
                (+ (ertn-stats-error-expected stats)
                   (ertn-stats-error-unexpected stats))
                (ertn-stats-error-unexpected stats)
                run-count
                (ertn-stats-total stats)))
       (insert
        (format "Started at:   %s\n"
                (ertn-format-time-iso8601 (ertn-stats-start-time stats))))
       ;; FIXME: This is ugly.  Need to properly define invariants of
       ;; the `stats' data structure.
       (let ((state (cond ((ertn-stats-aborted-p stats)
                           'aborted)
                          ((ertn-stats-current-test stats)
                           'running)
                          ((ertn-stats-end-time stats)
                           'finished)
                          (t
                           'preparing))))
         (ecase state
           (preparing
            (insert ""))
           (aborted
            (cond ((ertn-stats-current-test stats)
                   (insert "Aborted during test: ")
                   (ertn-insert-test-name-button
                    (ertn-test-name (ertn-stats-current-test stats))))
                  (t
                   (insert "Aborted."))))
           (running
            (assert (ertn-stats-current-test stats))
            (insert "Running test: ")
            (ertn-insert-test-name-button (ertn-test-name
                                          (ertn-stats-current-test stats))))
           (finished
            (assert (not (ertn-stats-current-test stats)))
            (insert "Finished.")))
         (insert "\n")
         (if (ertn-stats-end-time stats)
             (insert
              (format "%s%s\n"
                      (if (ertn-stats-aborted-p stats)
                          "Aborted at:   "
                        "Finished at:  ")
                      (ertn-format-time-iso8601 (ertn-stats-end-time stats))))
           (insert "\n"))
         (insert "\n"))
       (let ((progress-bar-string (with-current-buffer results-buffer
                                    ertn-results-progress-bar-string)))
         (let ((progress-bar-button-begin
                (insert-text-button (substring progress-bar-string 0 run-count)
                                    :type 'ertn-results-progress-bar-button)))
           (with-current-buffer results-buffer
             (set (make-local-variable 'ertn-results-progress-bar-button-begin)
                  progress-bar-button-begin)))
         (insert (substring progress-bar-string run-count)))
       (insert "\n\n")
       (buffer-string))
     ;; footer
     ;;
     ;; We actually want an empty footer, but that would trigger a bug
     ;; in ewoc, sometimes clearing the entire buffer.
     "\n")))

(defun ertn-results-update-stats-display (ewoc stats)
  "Update EWOC and the mode line to show data from STATS."
  (ertn-results-update-ewoc-hf ewoc stats)
  (force-mode-line-update)
  (redisplay t))

(defun ertn-char-for-test-result (result expectedp)
  "Return a character that represents the test result RESULT."
  (let ((char
         (etypecase result
           (ertn-test-passed ?.)
           (ertn-test-failed ?f)
           (ertn-test-error ?e)
           (null ?-)
           (ertn-test-aborted-with-non-local-exit ?a))))
    (if expectedp
        char
      (upcase char))))

(defun ertn-string-for-test-result (result expectedp)
  "Return a string that represents the test result RESULT."
  (etypecase result
    (ertn-test-passed "passed")
    (ertn-test-failed "failed")
    (ertn-test-error "error")
    (null "unknown")
    (ertn-test-aborted-with-non-local-exit "aborted")))

(defun ertn-tests-running-mode-line-indicator ()
  (let* ((stats ertn-current-run-stats)
         (tests-total (ertn-stats-total stats))
         (tests-completed (+ (ertn-stats-passed-expected stats)
                             (ertn-stats-passed-unexpected stats)
                             (ertn-stats-failed-expected stats)
                             (ertn-stats-failed-unexpected stats)
                             (ertn-stats-error-expected stats)
                             (ertn-stats-error-unexpected stats))))
    (if (>= tests-completed tests-total)
        (format " ERT(%s/%s,finished)" tests-completed tests-total)
      (format " ERT(%s/%s):%s"
              (1+ tests-completed)
              tests-total
              (if (null (ertn-stats-current-test stats))
                  "?"
                (format "%S"
                        (ertn-test-name (ertn-stats-current-test stats))))))))

(defun ertn-pp-with-indentation-and-newline (object)
  "Pretty-print OBJECT, indenting it to the current column of point.
Ensures a final newline is inserted."
  (let ((begin (point)))
    (pp object (current-buffer))
    (unless (bolp) (insert "\n"))
    (save-excursion
      (goto-char begin)
      (indent-sexp))))

(defun ertn-print-test-for-ewoc (entry)
  "The ewoc print function for ewoc test entries."
  (let* ((test (ertn-ewoc-entry-test entry))
         (result (ertn-ewoc-entry-result entry))
         (hiddenp (ertn-ewoc-entry-hidden-p entry))
         (expandedp (ertn-ewoc-entry-expanded-p entry))
         (extended-printer-limits-p (ertn-ewoc-entry-extended-printer-limits-p
                                     entry)))
    (cond (hiddenp)
          (t
           (insert-text-button (format "%c"
                                       (ertn-char-for-test-result
                                        result
                                        (ertn-test-result-expected-p test
                                                                    result)))
                               :type 'ertn-results-expand-collapse-button)
           (insert " ")
           (ertn-insert-test-name-button (ertn-test-name test))
           (insert "\n")
           (when (and expandedp (not (eql result 'nil)))
             (etypecase result
               (ertn-test-passed
                (insert "    passed\n")
                (insert ""))
               (ertn-test-result-with-condition
                (insert "    ")
                (let ((print-escape-newlines t)
                      (print-level (if extended-printer-limits-p 10 5))
                      (print-length (if extended-printer-limits-p 100 10)))
                  (let ((begin (point)))
                    (ertn-pp-with-indentation-and-newline
                     (ertn-test-result-with-condition-condition result))
                    (save-restriction
                      (narrow-to-region begin (point))
                      ;; Inhibit optimization in `debugger-make-xrefs'
                      ;; that sometimes inserts unrelated backtrace
                      ;; info into our buffer.
                      (let ((debugger-previous-backtrace nil))
                        (debugger-make-xrefs))))))
               (ertn-test-aborted-with-non-local-exit
                (insert "    aborted\n")))
             (insert "\n")))))
  nil)

(defun ertn-setup-results-buffer (stats listener buffer-name)
  "Set up a test results buffer."
  (unless buffer-name (setq buffer-name "*ert*"))
  (let ((buffer (let ((default-major-mode 'fundamental-mode))
                  (get-buffer-create buffer-name))))
    (with-current-buffer buffer
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (buffer-disable-undo)
        (erase-buffer)
        (ertn-results-mode)
        (set (make-local-variable 'ertn-results-ewoc)
             (ewoc-create 'ertn-print-test-for-ewoc nil nil t))
        (set (make-local-variable 'ertn-results-stats) stats)
        (set (make-local-variable 'ertn-results-progress-bar-string)
             (make-string (ertn-stats-total stats)
                          (ertn-char-for-test-result nil t)))
        (set (make-local-variable 'ertn-results-listener) listener)
        (ertn-results-update-ewoc-hf ertn-results-ewoc ertn-results-stats)
        (goto-char (1- (point-max)))
        buffer))))

(defun ertn-run-or-rerun-test (stats test listener)
  "Run the single test TEST and record the result using STATS and LISTENER."
  (let ((ertn-current-run-stats stats)
        (pos (ertn-stats-test-index stats test))
        (results (ertn-stats-test-results stats))
        (expected (ertn-stats-test-results-expected stats)))
    ;; Adjust stats to remove previous result.
    (if (aref expected pos)
        (etypecase (aref results pos)
          (ertn-test-passed (decf (ertn-stats-passed-expected stats)))
          (ertn-test-failed (decf (ertn-stats-failed-expected stats)))
          (ertn-test-error (decf (ertn-stats-error-expected stats)))
          (null)
          (ertn-test-aborted-with-non-local-exit))
      (etypecase (aref results pos)
        (ertn-test-passed (decf (ertn-stats-passed-unexpected stats)))
        (ertn-test-failed (decf (ertn-stats-failed-unexpected stats)))
        (ertn-test-error (decf (ertn-stats-error-unexpected stats)))
        (null)
        (ertn-test-aborted-with-non-local-exit)))
    (setf (aref results pos) nil)
    ;; Call listener after setting/before resetting
    ;; (ertn-stats-current-test stats); the listener might refresh the
    ;; mode line display, and if the value is not set yet/any more
    ;; during this refresh, the mode line will flicker unnecessarily.
    (setf (ertn-stats-current-test stats) test)
    (funcall listener 'test-started stats test)
    (setf (ertn-test-most-recent-result test) nil)
    (unwind-protect
        (ertn-run-test test)
      (let* ((result (ertn-test-most-recent-result test))
             (expectedp (typep result (ertn-test-expected-result-type test))))
        ;; Adjust stats to add new result.
        (if expectedp
            (etypecase result
              (ertn-test-passed (incf (ertn-stats-passed-expected stats)))
              (ertn-test-failed (incf (ertn-stats-failed-expected stats)))
              (ertn-test-error (incf (ertn-stats-error-expected stats)))
              (null)
              (ertn-test-aborted-with-non-local-exit))
          (etypecase result
            (ertn-test-passed (incf (ertn-stats-passed-unexpected stats)))
            (ertn-test-failed (incf (ertn-stats-failed-unexpected stats)))
            (ertn-test-error (incf (ertn-stats-error-unexpected stats)))
            (null)
            (ertn-test-aborted-with-non-local-exit)))
        (setf (aref results pos) result
              (aref expected pos) expectedp)
        (funcall listener 'test-ended stats test result))
      (setf (ertn-stats-current-test stats) nil))))

(defun ertn-run-tests (selector listener)
  "Run the tests specified by SELECTOR, sending progress updates to LISTENER."
  (let* ((tests (coerce (ertn-select-tests selector t) 'vector))
         (map (let ((map (make-hash-table :size (length tests))))
                (loop for i from 0
                      for test across tests
                      for key = (or (ertn-test-name test) test) do
                      (assert (not (gethash key map)))
                      (setf (gethash key map) i))
                map))
         (stats (make-ertn-stats :selector selector
                                :tests tests
                                :test-map map
                                :test-results (make-vector (length tests) nil)
                                :test-results-expected (make-vector
                                                        (length tests) nil)
                                :total (length tests)
                                :start-time (current-time))))
    (funcall listener 'run-started stats)
    (let ((abortedp t))
      (let ((ertn-current-run-stats stats))
        (force-mode-line-update)
        (unwind-protect
            (progn
              (loop for test across tests do
                    (ertn-run-or-rerun-test stats test listener))
              (setq abortedp nil))
          (setf (ertn-stats-aborted-p stats) abortedp)
          (setf (ertn-stats-end-time stats) (current-time))
          (funcall listener 'run-ended stats abortedp)))
      stats)))

(defun ertn-stats-test-index (stats test)
  "Return the index of TEST in the run represented by STATS."
  (gethash (or (ertn-test-name test) test) (ertn-stats-test-map stats)))

(defvar ertn-selector-history nil
  "List of recent test selectors read from terminal.")

;; Fix-me: return (regep (list of matches))?
;; Fix-me: Add prompt parameter?
(defun ertn-read-test-selector ()
  "Read a regexp for test selection from minibuffer.
The user can use TAB to see which tests match."
  (let* ((all-tests
          (mapcar (lambda (rec) (format "%s" (elt rec 1)))
                  (ertn-select-tests "" t))
          ;;'("ertn-group1-1" "ertn-group1-2" "ertn-other")
          )
         regexp
         ret
         (get-completions
          (lambda ()
            (let* ((ret (save-match-data
                          (mapcar (lambda (alt)
                                    (when (string-match regexp alt)
                                      alt))
                                  all-tests))))
              (setq ret (delq nil ret))
              ret))))
    (setq all-tests (append all-tests
                            '(":new"
                              ":failed" ":passed" ":error"
                              )
                            nil))
    (let ((mini-map (copy-keymap minibuffer-local-map)))
      (define-key mini-map [?\t]
        (lambda () (interactive)
          (with-output-to-temp-buffer "*Completions*"
            (display-completion-list
             (progn
               (setq regexp (minibuffer-contents))
               (set-text-properties 0 (length regexp) nil regexp)
               (funcall get-completions))))))
      (setq regexp
            (let* ((sym-here (thing-at-point 'symbol))
                   (test-here (when (and sym-here
                                         (memq sym-here all-tests))
                                sym-here))
                   (default (if sym-here
                                (substring-no-properties sym-here)
                              (if ertn-selector-history
                                  (first ertn-selector-history)
                                "t"))))
              (read-from-minibuffer
               (if (null default)
                   "Run tests, use TAB to see matches: "
                 (format "Run tests, use TAB to see matches (default %s): "
                         default))
               nil ;; initial-contents
               mini-map ;; keymap
               nil ;; read
               'ertn-selector-history
               default nil))))
    (setq ret regexp)
    (when (string= "t" ret)
      (setq ret t))
    ret))

;; Should OUTPUT-BUFFER-NAME and MESSAGE-FN really be arguments here?
;; They are needed only for our automated self-tests at the moment.
;; Or should there be some other mechanism?
;;;###autoload
(defun ertn-run-tests-interactively (selector
                                    &optional output-buffer-name message-fn)
  "Run the tests specified by SELECTOR and display the results in a buffer."
  (interactive
;;;    (list (let ((default (if ertn-selector-history
;;;                             (first ertn-selector-history)
;;;                           "t")))
;;;            (read-from-minibuffer (if (null default)
;;;                                      "Run tests: "
;;;                                    (format "Run tests (default %s): " default))
;;;                                  ;;nil nil t 'ertn-selector-history
;;;                                  ;;
;;;                                  ;; fix-me: seems like I am misunderstanding Christians intent here.
;;;                                  nil nil nil 'ertn-selector-history
;;;                                  default nil))
;;;          nil nil))
   (list (ertn-read-test-selector)
         nil nil))
  (unless message-fn (setq message-fn 'message))
  (lexical-let ((output-buffer-name output-buffer-name)
                buffer
                listener
                (message-fn message-fn))
    (setq listener
          (lambda (event-type &rest event-args)
            (ecase event-type
              (run-started
               (destructuring-bind (stats) event-args
                 (setq buffer (ertn-setup-results-buffer stats
                                                        listener
                                                        output-buffer-name))
                 (pop-to-buffer buffer)))
              (run-ended
               (destructuring-bind (stats abortedp) event-args
                 (funcall message-fn
                          "%sRan %s tests, %s results were as expected%s"
                          (if (not abortedp)
                              ""
                            "Aborted: ")
                          (ertn-stats-total stats)
                          (+ (ertn-stats-passed-expected stats)
                             (ertn-stats-failed-expected stats)
                             (ertn-stats-error-expected stats))
                          (let ((unexpected
                                 (+ (ertn-stats-passed-unexpected stats)
                                    (ertn-stats-failed-unexpected stats)
                                    (ertn-stats-error-unexpected stats))))
                            (if (zerop unexpected)
                                ""
                              (format ", %s unexpected" unexpected))))
                 (ertn-results-update-stats-display (with-current-buffer buffer
                                                     ertn-results-ewoc)
                                                   stats)))
              (test-started
               (destructuring-bind (stats test) event-args
                 (with-current-buffer buffer
                   (let* ((ewoc ertn-results-ewoc)
                          (pos (ertn-stats-test-index stats test))
                          (node (ewoc-nth ewoc pos)))
                     (unless node
                       ;; FIXME: How expensive is this assertion?
                       (assert (or (zerop pos) (ewoc-nth ewoc (1- pos)))
                               t)
                       (setq node (ewoc-enter-last
                                   ewoc
                                   (make-ertn-ewoc-entry :test test
                                                        :hidden-p t))))
                     (setf (ertn-ewoc-entry-test (ewoc-data node)) test)
                     (setf (ertn-ewoc-entry-result (ewoc-data node)) nil)
                     (aset ertn-results-progress-bar-string pos
                           (ertn-char-for-test-result nil t))
                     (ertn-results-update-stats-display ewoc stats)
                     (ewoc-invalidate ewoc node)))))
              (test-ended
               (destructuring-bind (stats test result) event-args
                 (with-current-buffer buffer
                   (let* ((ewoc ertn-results-ewoc)
                          (pos (ertn-stats-test-index stats test))
                          (node (ewoc-nth ewoc pos)))
                     (setf (ertn-ewoc-entry-result (ewoc-data node)) result)
                     (when (ertn-ewoc-entry-hidden-p (ewoc-data node))
                       (setf (ertn-ewoc-entry-hidden-p (ewoc-data node))
                             (ertn-test-result-expected-p test result)))
                     (aset ertn-results-progress-bar-string pos
                           (ertn-char-for-test-result result
                                                     (ertn-test-result-expected-p
                                                      test result)))
                     (ertn-results-update-stats-display ewoc stats)
                     (ewoc-invalidate ewoc node))))))))
    (ertn-run-tests
     selector
     listener)))

(defvar ertn-batch-backtrace-right-margin 70
  "*The maximum line length for printing backtraces in `ertn-run-tests-batch'.")

(defun ertn-run-tests-batch (selector)
  "Run the tests specified by SELECTOR, printing results to the terminal.

Returns the stats object."
  (ertn-run-tests
   selector
   (lambda (event-type &rest event-args)
     (ecase event-type
       (run-started
        (destructuring-bind (stats) event-args
          (message "Running %s tests (%s)"
                   (length (ertn-stats-tests stats))
                   (ertn-format-time-iso8601 (ertn-stats-start-time stats)))))
       (run-ended
        (destructuring-bind (stats abortedp) event-args
          (let ((unexpected (+ (ertn-stats-passed-unexpected stats)
                               (ertn-stats-failed-unexpected stats)
                               (ertn-stats-error-unexpected stats))))
            (message "\n%sRan %s tests, %s results were as expected%s (%s)\n"
                     (if (not abortedp)
                         ""
                       "Aborted: ")
                     (ertn-stats-total stats)
                     (+ (ertn-stats-passed-expected stats)
                        (ertn-stats-failed-expected stats)
                        (ertn-stats-error-expected stats))
                     (if (zerop unexpected)
                         ""
                       (format ", %s unexpected" unexpected))
                     (ertn-format-time-iso8601 (ertn-stats-end-time stats)))
            (unless (zerop unexpected)
              (message "%s unexpected results:" unexpected)
              (loop for test across (ertn-stats-tests stats)
                    for result = (ertn-test-most-recent-result test) do
                    (when (not (ertn-test-result-expected-p test result))
                      (message "%9s  %S"
                               (ertn-string-for-test-result result nil)
                               (ertn-test-name test))))
              (message "%s" "")))))
       (test-started
        )
       (test-ended
        (destructuring-bind (stats test result) event-args
          (etypecase result
            (ertn-test-passed)
            (ertn-test-result-with-condition
             (message "Test %S backtrace:" (ertn-test-name test))
             (with-temp-buffer
               (ertn-print-backtrace (ertn-test-result-with-condition-backtrace result))
               (goto-char (point-min))
               (while (not (eobp))
                 (let ((start (point))
                       (end (progn (end-of-line) (point))))
                   (setq end (min end
                                  (+ start ertn-batch-backtrace-right-margin)))
                   (message "%s" (buffer-substring-no-properties
                                  start end)))
                 (forward-line 1)))
             (with-temp-buffer
               (insert "  ")
               (let ((print-escape-newlines t)
                     (print-level 5)
                     (print-length 10))
                 (let ((begin (point)))
                   (ertn-pp-with-indentation-and-newline
                    (ertn-test-result-with-condition-condition result))))
               (goto-char (1- (point-max)))
               (assert (looking-at "\n"))
               (delete-char 1)
               (message "Test %S condition:" (ertn-test-name test))
               (message "%s" (buffer-string))))
            (ertn-test-aborted-with-non-local-exit))
          (let* ((max (prin1-to-string (length (ertn-stats-tests stats))))
                 (format-string (concat "%9s  %"
                                        (prin1-to-string (length max))
                                        "s/" max "  %S")))
            (message format-string
                     (ertn-string-for-test-result result
                                                 (ertn-test-result-expected-p
                                                  test result))
                     (1+ (ertn-stats-test-index stats test))
                     (ertn-test-name test)))))))))


;;; Commands and button actions for the results buffer.

(define-derived-mode ertn-results-mode fundamental-mode "ERTN-Results"
  "Major mode for viewing results of ERT test runs.")

(loop for (key binding) in
      '(("j" ertn-results-jump-between-summary-and-result)
        ("." ertn-results-find-test-at-point-other-window)
        ("r" ertn-results-rerun-test-at-point)
        ("d" ertn-results-rerun-test-at-point-debugging-errors)
        ("b" ertn-results-pop-to-backtrace-for-test-at-point)
        ("m" ertn-results-pop-to-messages-for-test-at-point)
        ("p" ertn-results-toggle-printer-limits-for-test-at-point)
        ("D" ertn-delete-test)
        ([?\t] forward-button)
        ([backtab] backward-button)
        )
      do
      (define-key ertn-results-mode-map key binding))

(define-button-type 'ertn-results-progress-bar-button
  'action #'ertn-results-progress-bar-button-action
  'help-echo "mouse-2, RET: Reveal test result")

(define-button-type 'ertn-test-name-button
  'action #'ertn-test-name-button-action
  'help-echo "mouse-2, RET: Find test definition")

(define-button-type 'ertn-results-expand-collapse-button
  'action #'ertn-results-expand-collapse-button-action
  'help-echo "mouse-2, RET: Expand/collapse test result")

(defun ertn-results-test-node-or-null-at-point ()
  "If point is on a valid ewoc node, return it; return nil otherwise.

To be used in the ERT results buffer."
  (let* ((ewoc ertn-results-ewoc)
         (node (ewoc-locate ewoc)))
    ;; `ewoc-locate' will return an arbitrary node when point is on
    ;; header or footer, or when all nodes are invisible.  So we need
    ;; to validate its return value here.
    (if (and (>= (point) (ewoc-location node))
             (not (ertn-ewoc-entry-hidden-p (ewoc-data node))))
        node
      nil)))

(defun ertn-results-test-node-at-point ()
  "If point is on a valid ewoc node, return it; signal an error otherwise.

To be used in the ERT results buffer."
  (or (ertn-results-test-node-or-null-at-point)
      (error "No test at point")))

(defun ertn-results-expand-collapse-button-action (button)
  "Expand or collapse the test node BUTTON belongs to."
  (let* ((ewoc ertn-results-ewoc)
         (node (save-excursion
                 (goto-char (ertn-button-action-position))
                 (ertn-results-test-node-at-point)))
         (entry (ewoc-data node)))
    (setf (ertn-ewoc-entry-expanded-p entry)
          (not (ertn-ewoc-entry-expanded-p entry)))
    (ewoc-invalidate ewoc node)))

(defun ertn-results-find-test-at-point-other-window ()
  "Find the definition of the test at point in another window.

To be used in the ERT results buffer."
  (interactive)
  (let* ((node (ertn-results-test-node-at-point))
         (entry (ewoc-data node))
         (test (ertn-ewoc-entry-test entry))
         (name (ertn-test-name test)))
    (ertn-find-test-other-window name)))

(defun ertn-test-name-button-action (button)
  "Find the definition of the test BUTTON belongs to, in another window."
  (let ((name (button-get button 'ertn-test-name)))
    (ertn-find-test-other-window name)))

(defun ertn-ewoc-position (ewoc node)
  "Return the position of NODE in EWOC, or nil if NODE is not in EWOC."
  (loop for i from 0
        for node-here = (ewoc-nth ewoc 0) then (ewoc-next ewoc node-here)
        do (when (eql node node-here)
             (return i))
        finally (return nil)))

(defun ertn-results-jump-between-summary-and-result ()
  "Jump back and forth between the test run summary and individual test results.

From an ewoc node, jumps to the character that represents the
same test in the progress bar, and vice versa.

To be used in the ERT results buffer."
  ;; Maybe this command isn't actually needed much, but if it is, it
  ;; seems like an indication that the UI design is not optimal.  If
  ;; jumping back and forth between a summary at the top of the buffer
  ;; and the error log in the remainder of the buffer is useful, then
  ;; the summary apparently needs to be easily accessible from the
  ;; error log, and perhaps it would be better to have it in a
  ;; separate buffer to keep it visible.
  (interactive)
  (let ((ewoc ertn-results-ewoc)
        (progress-bar-begin ertn-results-progress-bar-button-begin))
    (cond ((ertn-results-test-node-or-null-at-point)
           (let* ((node (ertn-results-test-node-at-point))
                  (pos (ertn-ewoc-position ewoc node)))
             (goto-char (+ progress-bar-begin pos))))
          ((and (<= progress-bar-begin (point))
                (< (point) (button-end (button-at progress-bar-begin))))
           (let* ((node (ewoc-nth ewoc (- (point) progress-bar-begin)))
                  (entry (ewoc-data node)))
             (when (ertn-ewoc-entry-hidden-p entry)
               (setf (ertn-ewoc-entry-hidden-p entry) nil)
               (ewoc-invalidate ewoc node))
             (ewoc-goto-node ewoc node)))
          (t
           (goto-char progress-bar-begin)))))

(defun ertn-button-action-position ()
  "The buffer position where the last button action was triggered."
  (cond ((integerp last-command-event)
         (point))
        ((eventp last-command-event)
         (posn-point (event-start last-command-event)))
        (t (assert nil))))

(defun ertn-results-progress-bar-button-action (button)
  "Find the ewoc node that represents the same test as the character clicked on."
  (goto-char (ertn-button-action-position))
  (ertn-results-jump-between-summary-and-result))

(defun ertn-results-rerun-test-at-point ()
  "Re-run the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((ewoc ertn-results-ewoc)
         (node (ertn-results-test-node-at-point))
         (entry (ewoc-data node))
         (old-test (ertn-ewoc-entry-test entry))
         (test-name (ertn-test-name old-test))
         ;; FIXME: Write a test for this lookup.
         (test (if test-name
                   (if (ertn-test-boundp test-name)
                       (ertn-get-test test-name)
                     (error "No such test: %S" test-name))
                 old-test))
         (stats ertn-results-stats)
         (pos (gethash test (ertn-stats-test-map stats)))
         (progress-message (format "Running test %S" (ertn-test-name test))))
    ;; Need to save and restore point manually here: When point is on
    ;; the first visible ewoc entry while the header is updated, point
    ;; moves to the top of the buffer.  This is undesirable, and a
    ;; simple `save-excursion' doesn't prevent it.
    (let ((point (point)))
      (unwind-protect
          (unwind-protect
              (progn
                (message "%s..." progress-message)
                (ertn-run-or-rerun-test stats test
                                       ertn-results-listener))
            (ertn-results-update-stats-display ewoc stats)
            (message "%s...%s"
                     progress-message
                     (let ((result (ertn-test-most-recent-result test)))
                       (ertn-string-for-test-result
                        result (ertn-test-result-expected-p test result)))))
        (goto-char point)))))

(defun ertn-results-rerun-test-at-point-debugging-errors ()
  "Re-run the test at point with `ertn-debug-on-error' bound to t.

To be used in the ERT results buffer."
  (interactive)
  (let ((ertn-debug-on-error t))
    (ertn-results-rerun-test-at-point)))

(defun ertn-print-backtrace (backtrace)
  "Format the backtrace BACKTRACE to the current buffer."
  ;; This is essentially a reimplementation of Fbacktrace
  ;; (src/eval.c), but for a saved backtrace, not the current one.
  (let ((print-escape-newlines t)
        (print-level 8)
        (print-length 50))
    (dolist (frame backtrace)
      (ecase (first frame)
        ((nil)
         ;; Special operator.
         (destructuring-bind (special-operator &rest arg-forms)
             (cdr frame)
           (insert
            (format "  %S\n" (list* special-operator arg-forms)))))
        ((t)
         ;; Function call.
         (destructuring-bind (fn &rest args) (cdr frame)
           (insert (format "  %S(" fn))
           (loop for firstp = t then nil
                 for arg in args do
                 (unless firstp
                   (insert " "))
                 (insert (format "%S" arg)))
           (insert ")\n")))))))

(defun ertn-results-pop-to-backtrace-for-test-at-point ()
  "Display the backtrace for the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((node (ertn-results-test-node-at-point))
         (entry (ewoc-data node))
         (test (ertn-ewoc-entry-test entry))
         (result (ertn-ewoc-entry-result entry)))
    (etypecase result
      (ertn-test-passed (error "Test passed, no backtrace available"))
      (ertn-test-result-with-condition
       (let ((backtrace (ertn-test-result-with-condition-backtrace result))
             (buffer
              (let ((default-major-mode 'fundamental-mode))
                (get-buffer-create "*ERT Backtrace*"))))
         (pop-to-buffer buffer)
         (setq buffer-read-only t)
         (let ((inhibit-read-only t))
           (erase-buffer)
           ;; Use unibyte because `debugger-setup-buffer' also does so.
           (set-buffer-multibyte nil)
           (setq truncate-lines t)
           (ertn-print-backtrace backtrace)
           (debugger-make-xrefs)
           (goto-char (point-min))))))))

(defun ertn-results-pop-to-messages-for-test-at-point ()
  "Display the part of the *Messages* buffer generated during the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((node (ertn-results-test-node-at-point))
         (entry (ewoc-data node))
         (test (ertn-ewoc-entry-test entry))
         (result (ertn-ewoc-entry-result entry)))
    (let ((buffer
           (let ((default-major-mode 'fundamental-mode))
             (get-buffer-create "*ERT Messages*"))))
      (pop-to-buffer buffer)
      (setq buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (ertn-test-result-messages result))
        (goto-char (point-min))
        (insert "Messages for test `")
        (ertn-insert-test-name-button (ertn-test-name test))
        (insert "':\n")))))

(defun ertn-results-toggle-printer-limits-for-test-at-point ()
  "Toggle how much of the condition to print for the test at point.

To be used in the ERT results buffer."
  (interactive)
  (let* ((ewoc ertn-results-ewoc)
         (node (ertn-results-test-node-at-point))
         (entry (ewoc-data node)))
    (setf (ertn-ewoc-entry-extended-printer-limits-p entry)
          (not (ertn-ewoc-entry-extended-printer-limits-p entry)))
    (ewoc-invalidate ewoc node)))

(defun ertn-activate-font-lock-keywords ()
  (font-lock-add-keywords
   nil
   '(("(\\(\\<ertn-deftest\\)\\>\\s *\\(\\sw+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))

(defun* ertn-remove-from-list (list-var element &key key test)
  "Remove ELEMENT from the value of LIST-VAR if present.

This is an inverse of `add-to-list'."
  (unless key (setq key #'identity))
  (unless test (setq test #'equal))
  (setf (symbol-value list-var)
        (remove* element
                 (symbol-value list-var)
                 :key key
                 :test test)))


;;; Actions on load/unload.

(add-to-list 'find-function-regexp-alist '(ertn-deftest . ertn-find-test-regexp))
(add-to-list 'minor-mode-alist '(ertn-current-run-stats
                                 (:eval
                                  (ertn-tests-running-mode-line-indicator))))
(add-to-list 'emacs-lisp-mode-hook 'ertn-activate-font-lock-keywords)

(defun ertn-unload-function ()
  (ertn-remove-from-list 'find-function-regexp-alist 'ertn-deftest :key #'car)
  (ertn-remove-from-list 'minor-mode-alist 'ertn-current-run-stats :key #'car)
  (ertn-remove-from-list 'emacs-lisp-mode-hook 'ertn-activate-font-lock-keywords)
  nil)

(defvar ertn-unload-hook '())
(add-hook 'ertn-unload-hook 'ertn-unload-function)


;;; Self-tests.

(ertn-delete-all-tests)

;; Test that test bodies are actually run.
(defvar ertn-test-body-was-run)
(ertn-deftest ertn-test-body-runs ()
  (setq ertn-test-body-was-run t))


;; Test that nested test bodies run.
(ertn-deftest ertn-nested-test-body-runs ()
  (lexical-let ((was-run nil))
    (let ((test (make-ertn-test :body (lambda ()
                                       (setq was-run t)))))
      (assert (not was-run))
      (ertn-run-test test)
      (assert was-run))))


;; Test that pass/fail works.
(ertn-deftest ertn-test-pass ()
  (let ((test (make-ertn-test :body (lambda ()))))
    (let ((result (ertn-run-test test)))
      (assert (typep result 'ertn-test-passed)))))

(ertn-deftest ertn-test-fail ()
  (let ((test (make-ertn-test :body (lambda () (ertn-fail "failure message")))))
    (let ((result (let ((ertn-debug-on-error nil))
                    (ertn-run-test test))))
      (assert (typep result 'ertn-test-failed) t)
      (assert (equal (ertn-test-result-with-condition-condition result)
                     '(ertn-test-failed "failure message"))
              t))))

(ertn-deftest ertn-test-fail-debug-with-condition-case ()
  (let ((test (make-ertn-test :body (lambda () (ertn-fail "failure message")))))
    (condition-case condition
        (progn
          (let ((ertn-debug-on-error t))
            (ertn-run-test test))
          (assert nil))
      ((error)
       (assert (equal condition '(ertn-test-failed "failure message")) t)))))

(ertn-deftest ertn-test-fail-debug-with-debugger-1 ()
  (let ((test (make-ertn-test :body (lambda () (ertn-fail "failure message")))))
    (let ((debugger (lambda (&rest debugger-args)
                      (assert nil))))
      (let ((ertn-debug-on-error nil))
        (ertn-run-test test)))))

(ertn-deftest ertn-test-fail-debug-with-debugger-2 ()
  (let ((test (make-ertn-test :body (lambda () (ertn-fail "failure message")))))
    (block nil
      (let ((debugger (lambda (&rest debugger-args)
                        (return-from nil nil))))
        (let ((ertn-debug-on-error t))
          (ertn-run-test test))
        (assert nil)))))

(ertn-deftest ertn-test-fail-debug-nested-with-debugger ()
  (let ((test (make-ertn-test :body (lambda ()
                                     (let ((ertn-debug-on-error t))
                                       (ertn-fail "failure message"))))))
    (let ((debugger (lambda (&rest debugger-args)
                      (assert nil nil "Assertion a"))))
      (let ((ertn-debug-on-error nil))
        (ertn-run-test test))))
  (let ((test (make-ertn-test :body (lambda ()
                                     (let ((ertn-debug-on-error nil))
                                       (ertn-fail "failure message"))))))
    (block nil
      (let ((debugger (lambda (&rest debugger-args)
                        (return-from nil nil))))
        (let ((ertn-debug-on-error t))
          (ertn-run-test test))
        (assert nil nil "Assertion b")))))

(ertn-deftest ertn-test-error ()
  (let ((test (make-ertn-test :body (lambda () (error "error message")))))
    (let ((result (let ((ertn-debug-on-error nil))
                    (ertn-run-test test))))
      (assert (typep result 'ertn-test-error) t)
      (assert (equal (ertn-test-result-with-condition-condition result)
                     '(error "error message"))
              t))))

(ertn-deftest ertn-test-error-debug ()
  (let ((test (make-ertn-test :body (lambda () (error "error message")))))
    (condition-case condition
        (progn
          (let ((ertn-debug-on-error t))
            (ertn-run-test test))
          (assert nil))
      ((error)
       (assert (equal condition '(error "error message")) t)))))


;; Test that `should' works.
(ertn-deftest ertn-test-should ()
  (let ((test (make-ertn-test :body (lambda () (ertn-should nil)))))
    (let ((result (let ((ertn-debug-on-error nil))
                    (ertn-run-test test))))
      (assert (typep result 'ertn-test-failed) t)
      (assert (equal (ertn-test-result-with-condition-condition result)
                     '(ertn-test-failed ((ertn-should nil) :form nil :value nil)))
              t)))
  (let ((test (make-ertn-test :body (lambda () (ertn-should t)))))
    (let ((result (ertn-run-test test)))
      (assert (typep result 'ertn-test-passed) t))))

(ertn-deftest ertn-test-should-value ()
  (ertn-should (eql (ertn-should 'foo) 'foo))
  (ertn-should (eql (ertn-should 'bar) 'bar)))

(ertn-deftest ertn-test-should-not ()
  (let ((test (make-ertn-test :body (lambda () (ertn-should-not t)))))
    (let ((result (let ((ertn-debug-on-error nil))
                    (ertn-run-test test))))
      (assert (typep result 'ertn-test-failed) t)
      (assert (equal (ertn-test-result-with-condition-condition result)
                     '(ertn-test-failed ((ertn-should-not t) :form t :value t)))
              t)))
  (let ((test (make-ertn-test :body (lambda () (ertn-should-not nil)))))
    (let ((result (ertn-run-test test)))
      (assert (typep result 'ertn-test-passed)))))


(ertn-deftest ertn-test-should-error ()
  ;; No error.
  (let ((test (make-ertn-test :body (lambda () (ertn-should-error (progn))))))
    (let ((result (let ((ertn-debug-on-error nil))
                    (ertn-run-test test))))
      (ertn-should (typep result 'ertn-test-failed))
      (ertn-should (equal (ertn-test-result-with-condition-condition result)
                     '(ertn-test-failed
                       ((ertn-should-error (progn))
                        :form (progn)
                        :value nil
                        :fail-reason "did not signal an error"))))))
  ;; A simple error.
  (let ((test (make-ertn-test :body (lambda () (ertn-should-error (error "foo"))))))
    (let ((result (ertn-run-test test)))
      (ertn-should (typep result 'ertn-test-passed))))
  ;; Error of unexpected type, no test.
  (let ((test (make-ertn-test :body (lambda ()
                                     (ertn-should-error (error "foo")
                                                   :type 'singularity-error)))))
    (let ((result (ertn-run-test test)))
      (ertn-should (typep result 'ertn-test-failed))
      (ertn-should (equal
               (ertn-test-result-with-condition-condition result)
               '(ertn-test-failed
                 ((ertn-should-error (error "foo") :type 'singularity-error)
                  :form (error "foo")
                  :condition (error "foo")
                  :fail-reason
                  "the error signalled did not have the expected type"))))))
  ;; Error of the expected type, no test.
  (let ((test (make-ertn-test :body (lambda ()
                                     (ertn-should-error (signal 'singularity-error
                                                           nil)
                                                   :type 'singularity-error)))))
    (let ((result (ertn-run-test test)))
      (ertn-should (typep result 'ertn-test-passed))))
  ;; Error that fails the test, no type.
  (let ((test (make-ertn-test :body (lambda ()
                                     (ertn-should-error
                                      (error "foo")
                                      :test (lambda (error) nil))))))
    (let ((result (ertn-run-test test)))
      (ertn-should (typep result 'ertn-test-failed))
      (ertn-should (equal (ertn-test-result-with-condition-condition result)
                     '(ertn-test-failed
                       ((ertn-should-error (error "foo") :test (lambda (error) nil))
                        :form (error "foo")
                        :condition (error "foo")
                        :fail-reason
                        "the error signalled did not pass the test"))))))
  ;; Error that passes the test, no type.
  (let ((test (make-ertn-test :body (lambda ()
                                     (ertn-should-error (error "foo")
                                                   :test (lambda (error) t))))))
    (let ((result (ertn-run-test test)))
      (ertn-should (typep result 'ertn-test-passed))))
  ;; Error that has the expected type but fails the test.
  (let ((test (make-ertn-test :body (lambda ()
                                     (ertn-should-error
                                      (signal 'singularity-error nil)
                                      :type 'singularity-error
                                      :test (lambda (error) nil))))))
    (let ((result (ertn-run-test test)))
      (ertn-should (typep result 'ertn-test-failed))
      (ertn-should (equal (ertn-test-result-with-condition-condition result)
                     '(ertn-test-failed
                       ((ertn-should-error (signal 'singularity-error nil)
                                      :type 'singularity-error
                                      :test (lambda (error) nil))
                        :form (signal singularity-error nil)
                        :condition (singularity-error)
                        :fail-reason
                        "the error signalled did not pass the test"))))))
  ;; Error that has the expected type and passes the test.
  (let ((test (make-ertn-test :body (lambda ()
                                     (ertn-should-error
                                      (signal 'singularity-error nil)
                                      :type 'singularity-error
                                      :test (lambda (error) t))))))
    (let ((result (ertn-run-test test)))
      (ertn-should (typep result 'ertn-test-passed))))
  )

(ertn-deftest ertn-test-should-error-subtypes ()
  (let ((test (make-ertn-test
               :body (lambda ()
                       (ertn-should-error (signal 'singularity-error nil)
                                     :type 'singularity-error
                                     :exclude-subtypes t)))))
    (let ((result (ertn-run-test test)))
      (ertn-should (typep result 'ertn-test-passed))))
  (let ((test (make-ertn-test
               :body (lambda ()
                       (ertn-should-error (signal 'arith-error nil)
                                     :type 'singularity-error)))))
    (let ((result (ertn-run-test test)))
      (ertn-should (typep result 'ertn-test-failed))
      (ertn-should (equal
               (ertn-test-result-with-condition-condition result)
               '(ertn-test-failed
                 ((ertn-should-error (signal 'arith-error nil)
                                :type 'singularity-error)
                  :form (signal arith-error nil)
                  :condition (arith-error)
                  :fail-reason
                  "the error signalled did not have the expected type"))))))
  (let ((test (make-ertn-test
               :body (lambda ()
                       (ertn-should-error (signal 'arith-error nil)
                                     :type 'singularity-error
                                     :exclude-subtypes t)))))
    (let ((result (ertn-run-test test)))
      (ertn-should (typep result 'ertn-test-failed))
      (ertn-should (equal
               (ertn-test-result-with-condition-condition result)
               '(ertn-test-failed
                 ((ertn-should-error (signal 'arith-error nil)
                                :type 'singularity-error
                                :exclude-subtypes t)
                  :form (signal arith-error nil)
                  :condition (arith-error)
                  :fail-reason
                  "the error signalled did not have the expected type"))))))
  (let ((test (make-ertn-test
               :body (lambda ()
                       (ertn-should-error (signal 'singularity-error nil)
                                     :type 'arith-error
                                     :exclude-subtypes t)))))
    (let ((result (ertn-run-test test)))
      (ertn-should (typep result 'ertn-test-failed))
      (ertn-should (equal
               (ertn-test-result-with-condition-condition result)
               '(ertn-test-failed
                 ((ertn-should-error (signal 'singularity-error nil)
                                :type 'arith-error
                                :exclude-subtypes t)
                  :form (signal singularity-error nil)
                  :condition (singularity-error)
                  :fail-reason
                  "the error signalled was a subtype of the expected type"))))))
  )

;; Test that `should' errors contain the information we expect them to.
(defmacro ertn-test-my-list (&rest args)
  `(list ,@args))

(ertn-deftest ertn-test-should-failure-debugging ()
  (loop for (body expected-condition) in
        `((,(lambda () (let ((x nil)) (ertn-should x)))
           (ertn-test-failed ((ertn-should x) :form x :value nil)))
          (,(lambda () (let ((x t)) (ertn-should-not x)))
           (ertn-test-failed ((ertn-should-not x) :form x :value t)))
          (,(lambda () (let ((x t)) (ertn-should (not x))))
           (ertn-test-failed ((ertn-should (not x)) :form (not t) :value nil)))
          (,(lambda () (let ((x nil)) (ertn-should-not (not x))))
           (ertn-test-failed ((ertn-should-not (not x)) :form (not nil) :value t)))
          (,(lambda () (let ((x t) (y nil)) (ertn-should-not (ertn-test-my-list x y))))
           (ertn-test-failed
            ((ertn-should-not (ertn-test-my-list x y))
             :form (list t nil)
             :value (t nil))))
          (,(lambda () (let ((x t)) (ertn-should (error "foo"))))
           (error "foo")))
        do
        (let ((test (make-ertn-test :body body)))
          (condition-case actual-condition
              (progn
                (let ((ertn-debug-on-error t))
                  (ertn-run-test test))
                (assert nil))
            ((error)
             (ertn-should (equal actual-condition expected-condition)))))))

(ertn-deftest ertn-test-messages ()
  (let* ((message-string "Test message")
         (messages-buffer (get-buffer-create "*Messages*"))
         (test (make-ertn-test :body (lambda () (message "%s" message-string)))))
    (with-current-buffer messages-buffer
      (let ((result (ertn-run-test test)))
        (ertn-should (equal (concat message-string "\n")
                       (ertn-test-result-messages result)))))))

(defun ertn-call-with-temporary-messages-buffer (thunk)
  (lexical-let ((new-buffer-name (generate-new-buffer-name
                                  "*Messages* orig buffer")))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create "*Messages*")
            (rename-buffer new-buffer-name))
          (get-buffer-create "*Messages*")
          (funcall thunk))
      (kill-buffer "*Messages*")
      (with-current-buffer new-buffer-name
        (rename-buffer "*Messages*")))))

(ertn-deftest ertn-test-messages-on-log-truncation ()
  (let ((test (make-ertn-test
               :body (lambda ()
                       ;; Emacs would combine messages if we
                       ;; generate the same message multiple
                       ;; times.
                       (message "a")
                       (message "b")
                       (message "c")
                       (message "d")))))
    (let (result)
      (ertn-call-with-temporary-messages-buffer
       (lambda ()
         (let ((message-log-max 2))
           (setq result (ertn-run-test test)))
         (ertn-should (equal (with-current-buffer "*Messages*"
                          (buffer-string))
                        "c\nd\n"))))
      (ertn-should (equal (ertn-test-result-messages result) "a\nb\nc\nd\n")))))

;; Test `ertn-select-tests'.
(ertn-deftest ertn-test-select-regexp ()
  (ertn-should (equal (ertn-select-tests "^ertn-test-select-regexp$" t)
                 (list (ertn-get-test 'ertn-test-select-regexp)))))

(ertn-deftest ertn-test-test-boundp ()
  (ertn-should (ertn-test-boundp 'ertn-test-test-boundp))
  (ertn-should-not (ertn-test-boundp (make-symbol "ertn-not-a-test"))))

(ertn-deftest ertn-test-select-member ()
  (ertn-should (equal (ertn-select-tests '(member ertn-test-select-member) t)
                 (list (ertn-get-test 'ertn-test-select-member)))))

(ertn-deftest ertn-test-select-test ()
  (ertn-should (equal (ertn-select-tests (ertn-get-test 'ertn-test-select-test) t)
                 (list (ertn-get-test 'ertn-test-select-test)))))

(ertn-deftest ertn-test-select-symbol ()
  (ertn-should (equal (ertn-select-tests 'ertn-test-select-symbol t)
                 (list (ertn-get-test 'ertn-test-select-symbol)))))

(ertn-deftest ertn-test-select-and ()
  (let ((test (make-ertn-test
               :name nil
               :body nil
               :most-recent-result (make-ertn-test-failed
                                    :condition nil
                                    :backtrace nil))))
    (ertn-should (equal (ertn-select-tests `(and (member ,test) :failed) t)
                   (list test)))))


;; Test utility functions.
(ertn-deftest ertn-proper-list-p ()
  (ertn-should (ertn-proper-list-p '()))
  (ertn-should (ertn-proper-list-p '(1)))
  (ertn-should (ertn-proper-list-p '(1 2)))
  (ertn-should (ertn-proper-list-p '(1 2 3)))
  (ertn-should (ertn-proper-list-p '(1 2 3 4)))
  (ertn-should (not (ertn-proper-list-p 'a)))
  (ertn-should (not (ertn-proper-list-p '(1 . a))))
  (ertn-should (not (ertn-proper-list-p '(1 2 . a))))
  (ertn-should (not (ertn-proper-list-p '(1 2 3 . a))))
  (ertn-should (not (ertn-proper-list-p '(1 2 3 4 . a))))
  (let ((a (list 1)))
    (setf (cdr (last a)) a)
    (ertn-should (not (ertn-proper-list-p a))))
  (let ((a (list 1 2)))
    (setf (cdr (last a)) a)
    (ertn-should (not (ertn-proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) a)
    (ertn-should (not (ertn-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) a)
    (ertn-should (not (ertn-proper-list-p a))))
  (let ((a (list 1 2)))
    (setf (cdr (last a)) (cdr a))
    (ertn-should (not (ertn-proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) (cdr a))
    (ertn-should (not (ertn-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cdr a))
    (ertn-should (not (ertn-proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) (cddr a))
    (ertn-should (not (ertn-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cddr a))
    (ertn-should (not (ertn-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cdddr a))
    (ertn-should (not (ertn-proper-list-p a)))))

(ertn-deftest ertn-parse-keys-and-body ()
  (ertn-should (equal (ertn-parse-keys-and-body "doc" '(foo))
                     '(nil "doc" (foo))))
  (ertn-should (equal (ertn-parse-keys-and-body "doc" '(:bar foo))
                     '((:bar foo) "doc" nil)))
  (ertn-should (equal (ertn-parse-keys-and-body nil '(:bar foo))
                     '((:bar foo) nil nil)))
  (ertn-should (equal (ertn-parse-keys-and-body "doc" '(:bar foo))
                     '((:bar foo) "doc" nil)))
  (ertn-should (equal (ertn-parse-keys-and-body nil '(:bar foo a (b)))
                     '((:bar foo) nil (a (b)))))
  (ertn-should (equal (ertn-parse-keys-and-body nil '(:bar foo :a (b)))
                     '((:bar foo :a (b)) nil nil)))
  (ertn-should (equal (ertn-parse-keys-and-body nil '(bar foo :a (b)))
                     '(nil nil (bar foo :a (b)))))
  (ertn-should-error (ertn-parse-keys-and-body nil '(:bar foo :a))))



;; Test `ertn-run-tests'.
(ertn-deftest ertn-test-run-tests ()
  (let ((passing-test (make-ertn-test :name 'passing-test
                                     :body (lambda () (ertn-pass))))
        (failing-test (make-ertn-test :name 'failing-test
                                     :body (lambda () (ertn-fail
                                                       "failure message"))))
        )
    (let ((ertn-debug-on-error nil))
      (let* ((buffer-name (generate-new-buffer-name " *ertn-test-run-tests*"))
             (messages nil)
             (mock-message-fn
              (lambda (format-string &rest args)
                (push (apply #'format format-string args) messages))))
        (save-window-excursion
          (unwind-protect
              (let ((case-fold-search nil))
                (ertn-run-tests-interactively
                 `(member ,passing-test ,failing-test) buffer-name
                 mock-message-fn)
                (ertn-should (equal messages `(,(concat
                                            "Ran 2 tests, 1 results were "
                                            "as expected, 1 unexpected"))))
                (with-current-buffer buffer-name
                  (goto-char (point-min))
                  (ertn-should (equal
                           (buffer-substring (point-min)
                                             (save-excursion
                                               (forward-line 5)
                                               (point)))
                           (concat
                            "Selector: (member <passing-test> <failing-test>)\n"
                            "Passed: 1 (0 unexpected)\n"
                            "Failed: 1 (1 unexpected)\n"
                            "Error:  0 (0 unexpected)\n"
                            "Total:  2/2\n")))))
            (when (get-buffer buffer-name)
              (kill-buffer buffer-name))))))))

(ertn-deftest ertn-test-special-operator-p ()
  (ertn-should (ertn-special-operator-p 'if))
  (ertn-should-not (ertn-special-operator-p 'car))
  (ertn-should-not (ertn-special-operator-p 'ertn-special-operator-p))
  (let ((b (gensym)))
    (ertn-should-not (ertn-special-operator-p b))
    (fset b 'if)
    (ertn-should (ertn-special-operator-p b))))

;; This test attempts to demonstrate that there is no way to force
;; immediate truncation of the *Messages* buffer from Lisp (and hence
;; justifies the existence of
;; `ertn-force-message-log-buffer-truncation'): The only way that came
;; to my mind was (message ""), which doesn't have the desired effect.
(ertn-deftest ertn-test-builtin-message-log-flushing ()
  (ertn-call-with-temporary-messages-buffer
   (lambda ()
     (with-current-buffer "*Messages*"
       (let ((message-log-max 2))
         (let ((message-log-max t))
           (loop for i below 4 do
                 (message "%s" i))
           (ertn-should (eql (count-lines (point-min) (point-max)) 4)))
         (ertn-should (eql (count-lines (point-min) (point-max)) 4))
         (message "")
         (ertn-should (eql (count-lines (point-min) (point-max)) 4))
         (message "Test message")
         (ertn-should (eql (count-lines (point-min) (point-max)) 2)))))))

(ertn-deftest ertn-test-force-message-log-buffer-truncation ()
  (labels ((body ()
             (loop for i below 5 do
                   (message "%s" i)))
           (c (x)
             (ertn-call-with-temporary-messages-buffer
              (lambda ()
                (let ((message-log-max x))
                  (body))
                (with-current-buffer "*Messages*"
                  (buffer-string)))))
           (lisp (x)
             (ertn-call-with-temporary-messages-buffer
              (lambda ()
                (let ((message-log-max t))
                  (body))
                (let ((message-log-max x))
                  (ertn-force-message-log-buffer-truncation))
                (with-current-buffer "*Messages*"
                  (buffer-string))))))
    (loop for x in '(0 1 2 3 4 5 6 t) do
          (ertn-should (equal (c x) (lisp x))))))

(defun ertn-run-self-tests ()
  ;; Run tests and make sure they actually ran.
  (let ((window-configuration (current-window-configuration)))
    (let ((ertn-test-body-was-run nil))
      ;; The buffer name chosen here should not compete with the default
      ;; results buffer name for completion in `switch-to-buffer'.
      (let ((stats (ertn-run-tests-interactively "^ertn-" " *ert self-tests*")))
        (assert ertn-test-body-was-run)
        (when (zerop (+ (ertn-stats-passed-unexpected stats)
                        (ertn-stats-failed-unexpected stats)
                        (ertn-stats-error-unexpected stats)))
          ;; Hide results window only when everything went well.
          (set-window-configuration window-configuration))))))

(provide 'ertn)

;;; ertn.el ends here
