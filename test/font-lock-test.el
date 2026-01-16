;;; font-lock-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2025, 2026 Stefan Möding

;; Author: Stefan Möding
;; Created: <2025-01-03 09:45:33 stm>
;; Updated: <2026-01-20 15:03:59 stm>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit test suite for mathprog-ts-mode

;;; Code:

(message "Running Emacs %s with tests from %s"
         emacs-version (file-relative-name load-file-name))


;;; Requirements
(require 'ert)

(declare-function mathprog-test-with-temp-buffer (content &rest body))
(declare-function mathprog-test-face-at (pos &optional content))


;;; Set

(ert-deftest fontify/set ()
  (mathprog-test-with-temp-buffer "set nodes;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))
    (should (eq (mathprog-test-face-at 5) 'mathprog-ts-variable-name))))

(ert-deftest fontify/set-with-superset ()
  (mathprog-test-with-temp-buffer "set arcs within nodes cross nodes;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))
    (should (eq (mathprog-test-face-at 5) 'mathprog-ts-variable-name))
    (should (eq (mathprog-test-face-at 10) 'mathprog-ts-attribute))
    (should (eq (mathprog-test-face-at 17) 'mathprog-ts-variable-use))
    (should (eq (mathprog-test-face-at 23) 'mathprog-ts-keyword))))

(ert-deftest fontify/set-with-setof-condition ()
  (mathprog-test-with-temp-buffer "set C := setof{a in A, b in B[a]} b;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))
    (should (eq (mathprog-test-face-at 5) 'mathprog-ts-variable-name))
    (should (eq (mathprog-test-face-at 10) 'mathprog-ts-iterated-operator))
    (should (eq (mathprog-test-face-at 16) 'mathprog-ts-variable-use))
    (should (eq (mathprog-test-face-at 18) 'mathprog-ts-keyword))
    (should (eq (mathprog-test-face-at 21) 'mathprog-ts-variable-use))
    (should (eq (mathprog-test-face-at 29) 'mathprog-ts-variable-use))
    (should (eq (mathprog-test-face-at 31) 'mathprog-ts-variable-use))
    (should (eq (mathprog-test-face-at 35) 'mathprog-ts-variable-use))))


;;; Numbers

(ert-deftest fontify/number-integer ()
  (mathprog-test-with-temp-buffer "param x := 42;"
    (should (eq (mathprog-test-face-at 13) 'mathprog-ts-number))))

(ert-deftest fontify/number-float ()
  (mathprog-test-with-temp-buffer "param x := 4.2;"
    (should (eq (mathprog-test-face-at 13) 'mathprog-ts-number))))

(ert-deftest fontify/number-scientific ()
  (mathprog-test-with-temp-buffer "param x := 4.2e12;"
    (should (eq (mathprog-test-face-at 13) 'mathprog-ts-number))
    (should (eq (mathprog-test-face-at 14) 'mathprog-ts-number))
    (should (eq (mathprog-test-face-at 15) 'mathprog-ts-number))
    (should (eq (mathprog-test-face-at 16) 'mathprog-ts-number))))

(ert-deftest fontify/number-negative ()
  (mathprog-test-with-temp-buffer "param x := -42;"
    (should (eq (mathprog-test-face-at 12) 'mathprog-ts-number))
    (should (eq (mathprog-test-face-at 13) 'mathprog-ts-number))))


;;; Comments

(ert-deftest fontify/comment-one-line ()
  (mathprog-test-with-temp-buffer "# comment"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-comment))
    (should (eq (mathprog-test-face-at 3) 'mathprog-ts-comment))))

(ert-deftest fontify/comment-multiple-lines ()
  (mathprog-test-with-temp-buffer "/*\n*\n*/"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-comment))
    (should (eq (mathprog-test-face-at 2) 'mathprog-ts-comment))
    (should (eq (mathprog-test-face-at 4) 'mathprog-ts-comment))
    (should (eq (mathprog-test-face-at 5) 'mathprog-ts-comment))
    (should (eq (mathprog-test-face-at 6) 'mathprog-ts-comment))))


;;; Statements

(ert-deftest fontify/statement-set ()
  (mathprog-test-with-temp-buffer "set x;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-param ()
  (mathprog-test-with-temp-buffer "param x := 1;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-var ()
  (mathprog-test-with-temp-buffer "var x binary;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-subject-to ()
  (mathprog-test-with-temp-buffer "s.t. r: x+y <= 1;
subj to r: x+y <= 1;
subject to r: x+y <= 1;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))
    (should (eq (mathprog-test-face-at 19) 'mathprog-ts-statement))
    (should (eq (mathprog-test-face-at 24) 'mathprog-ts-statement))
    (should (eq (mathprog-test-face-at 40) 'mathprog-ts-statement))
    (should (eq (mathprog-test-face-at 48) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-minimize ()
  (mathprog-test-with-temp-buffer "minimize obj: x + 1.5 * (y + z);"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-maximize ()
  (mathprog-test-with-temp-buffer "maximize obj: x + 1.5 * (y + z);"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-solve ()
  (mathprog-test-with-temp-buffer "solve;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-check ()
  (mathprog-test-with-temp-buffer "check x+y > 42;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-display ()
  (mathprog-test-with-temp-buffer "display 'x=', x;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-printf ()
  (mathprog-test-with-temp-buffer "printf 'Hello, world!\n';"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-for ()
  (mathprog-test-with-temp-buffer "for {(i,j) in E} { check x[i,j] >= 0; }"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-end ()
  (mathprog-test-with-temp-buffer "end;
data;
end;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))
    (should (eq (mathprog-test-face-at 6) 'mathprog-ts-statement))
    (should (eq (mathprog-test-face-at 12) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-data-set ()
  (mathprog-test-with-temp-buffer "data;
set month := Jan Feb Mar;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))
    (should (eq (mathprog-test-face-at 7) 'mathprog-ts-statement))))

(ert-deftest fontify/statement-data-param ()
  (mathprog-test-with-temp-buffer "data;
param month := Jan;"
    (should (eq (mathprog-test-face-at 1) 'mathprog-ts-statement))
    (should (eq (mathprog-test-face-at 7) 'mathprog-ts-statement))))

;;; Values

(ert-deftest fontify/data-values ()
  (mathprog-test-with-temp-buffer "data;
param value := iron -.1, \"nickel\" .02;"
    (should (eq (mathprog-test-face-at 13) 'mathprog-ts-variable-name))
    (should (eq (mathprog-test-face-at 22) 'mathprog-ts-datavalue))
    (should (eq (mathprog-test-face-at 27) 'mathprog-ts-number))
    (should (eq (mathprog-test-face-at 33) 'mathprog-ts-string))
    (should (eq (mathprog-test-face-at 42) 'mathprog-ts-number))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; font-lock-test.el ends here
