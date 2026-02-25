;;; mathprog-ts-mode.el --- Major mode for the GNU MathProg modeling language -*- lexical-binding: t; -*-
;;
;; Author:           Stefan Möding <stm@kill-9.net>
;; Maintainer:       Stefan Möding <stm@kill-9.net>
;; Version:          0.1.0
;; Created:          <2026-01-06 19:55:01 stm>
;; Updated:          <2026-02-25 16:47:46 stm>
;; URL:              https://github.com/smoeding/mathprog-ts-mode
;; Keywords:         languages
;; Package-Requires: ((emacs "29.1"))

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

;; GNU MathProg is a modeling language to describe optimization problems
;; using linear programming (LP) and mixed integer programming (MIP) models.
;; This package uses a Tree-sitter parser to provide syntax highlighting and
;; navigation for the MathProg language used by the GNU Linear Programming
;; Kit (GLPK).

;; The mode does not add any file name patterns to `auto-mode-alist'.
;; Usually the file extension '.mod' is used for a MathProg model file, but
;; that is already occupied for `m2-mode'.  You can either add the preferred
;; file name pattern in your personal Emacs configuration or use file local
;; variables to automatically enable the mode when visiting a model file.

;; The package uses a Tree-sitter library to parse MathProg code and you
;; need to install the appropriate parser (which is defined by the constant
;; `mathprog-ts-mode-treesit-language-source').  This can for example be
;; done by using this Elisp code:
;;
;;    (require 'mathprog-ts-mode)
;;    (mathprog-ts-mode-install-grammar)
;;
;; Note that a C compiler is required for this step.  Using the function
;; provided by the package ensures that a version of the parser matching the
;; package will be installed.  These commands should also be used to update
;; the parser to the correct version when the package is updated.

;;; Code:


;;;; Requirements

(require 'treesit)

(eval-when-compile
  (require 'rx))


;;;; Customization

(defgroup mathprog-ts nil
  "Write linear programming problems using MathProg in Emacs."
  :prefix "mathprog-ts-"
  :group 'languages
  :link '(url-link :tag "Repository"
                   "https://github.com/smoeding/mathprog-ts-mode"))

(defcustom mathprog-ts-indent-level 2
  "Number of spaces for each indentation step."
  :group 'mathprog-ts
  :type 'integer
  :safe 'integerp)

(defcustom mathprog-ts-indent-tabs-mode nil
  "Indentation can insert tabs in MathProg mode if this is non-nil."
  :group 'mathprog-ts
  :type 'boolean
  :safe 'booleanp)

(defcustom mathprog-ts-solver-program "glpsol"
  "The program name of the MathProg solver to use."
  :group 'mathprog-ts
  :type 'string
  :local t)

(defcustom mathprog-ts-solver-check-args '("--check" "--math")
  "List of arguments passed to the solver when checking a model."
  :group 'mathprog-ts
  :type '(list string)
  :local t)


;;;; Internals

(defconst mathprog-ts--keywords-regexp
  (rx (seq bow
           (or "and" "by" "cross" "diff" "div" "in" "inter" "less"
               "mod" "not" "or" "symdiff" "union" "within")
           eow))
  "Regexp of reserved keywords used in MathProg.")

(defconst mathprog-ts--statements-regexp
  (rx (seq bow
           (or "set" "param" "var" "constraint" "objective" "check"
               "display" "printf" "for" "table" "solve" "end" "data"
               "set_data" "param_data")
           eow))
  "Regexp of parser nodes that start a new MathProg statement.")

(defconst mathprog-ts--attributes
  '("default" "dimen" "within" "in"
    "integer" "binary" "logical" "symbolic")
  "List of attributes used for MathProg sets, parameters and variables.")


;;;; Faces

(defface mathprog-ts-datavalue
  '((t :inherit font-lock-constant-face))
  "Face used for unquoted values in the data section."
  :group 'mathprog-ts)

(defface mathprog-ts-statement
  '((t :inherit font-lock-preprocessor-face))
  "Face used for MathProg model declaration and functional statements."
  :group 'mathprog-ts)

(defface mathprog-ts-iterated-operator
  '((t :inherit font-lock-builtin-face))
  "Face used for operators in iterated expressions.")

(defface mathprog-ts-keyword
  '((t :inherit font-lock-keyword-face))
  "Face used for MathProg keywords."
  :group 'mathprog-ts)

(defface mathprog-ts-attribute
  '((t :inherit font-lock-type-face))
  "Face used for attribute types."
  :group 'mathprog-ts)

(defface mathprog-ts-variable-name
  '((t :inherit font-lock-variable-name-face))
  "Face used for the name of a MathProg variable."
  :group 'mathprog-ts)

(defface mathprog-ts-variable-use
  '((t :inherit font-lock-variable-use-face))
  "Face used for the name of a MathProg variable being referenced."
  :group 'mathprog-ts)

(defface mathprog-ts-function-call
  '((t :inherit font-lock-function-call-face))
  "Face used for the name of a function being called in MathProg."
  :group 'mathprog-ts)

(defface mathprog-ts-operator
  '((t :inherit font-lock-operator-face))
  "Face used for MathProg operators."
  :group 'mathprog-ts)

(defface mathprog-ts-suffix
  '((t :inherit font-lock-property-use-face))
  "Face used for object suffixes in MathProg."
  :group 'mathprog-ts)


;;;; Font-Lock

(defvar mathprog-ts-mode-feature-list
  ;; Level 1 usually contains only comments and definitions.
  ;; Level 2 usually adds keywords, strings, data types, etc.
  ;; Level 3 usually represents full-blown fontifications, including
  ;; assignments, constants, numbers and literals, etc.
  ;; Level 4 adds everything else that can be fontified: delimiters,
  ;; operators, brackets, punctuation, all functions, properties,
  ;; variables, etc.
  '((comment)
    (keyword string builtin)
    (number datavalue function attribute variable suffix)
    (operator error))
  "`treesit-font-lock-feature-list' for `mathprog-ts-mode'.")

(defvar mathprog-ts-mode-font-lock-settings
  `( ;;
    :feature comment
    :language mathprog
    ((comment) @font-lock-comment-face)

    :feature string
    :language mathprog
    ((string) @font-lock-string-face)

    :feature number
    :language mathprog
    ((number) @font-lock-number-face)

    :feature datavalue
    :language mathprog
    ((bareword) @mathprog-ts-datavalue)

    :feature attribute
    :language mathprog
    ((attribute [,@mathprog-ts--attributes] @mathprog-ts-attribute))

    :feature keyword
    :language mathprog
    (((operator) @mathprog-ts-keyword
      (:match ,mathprog-ts--keywords-regexp @mathprog-ts-keyword))
     ((conditional_expression ["if" "then" "else"] @mathprog-ts-keyword)))

    :feature builtin
    :language mathprog
    ((set "set" @mathprog-ts-statement)
     (param "param" @mathprog-ts-statement)
     (var "var" @mathprog-ts-statement)
     (constraint ["s.t." "subj" "subject" "to"] @mathprog-ts-statement)
     (objective ["maximize" "minimize"] @mathprog-ts-statement)
     (solve "solve" @mathprog-ts-statement)
     (check "check" @mathprog-ts-statement)
     (display "display" @mathprog-ts-statement)
     (printf "printf" @mathprog-ts-statement)
     (for "for" @mathprog-ts-statement)
     (table "table" @mathprog-ts-statement)
     (end "end" @mathprog-ts-statement)
     (data "data" @mathprog-ts-statement)
     (set_data "set" @mathprog-ts-statement)
     (param_data "param" @mathprog-ts-statement)
     (iterated_expression (operator) @mathprog-ts-iterated-operator))

    :feature function
    :language mathprog
    ((function_name) @mathprog-ts-function-call)

    :feature suffix
    :language mathprog
    ((suffix) @mathprog-ts-suffix)

    :feature variable
    :language mathprog
    (((model_object) @mathprog-ts-variable-name)
     ((symbolic_name) @mathprog-ts-variable-use))

    :feature operator
    :language mathprog
    (((operator) @font-lock-negation-char-face
      (:match "!" @font-lock-negation-char-face))
     ((operator) @mathprog-ts-operator))

    :feature error
    :language mathprog
    :override t
    ((ERROR) @font-lock-warning-face)))


;;;; Checking

(defun mathprog-ts-check-buffer ()
  "Check the MathProg model in the current buffer."
  (interactive)
  (let* ((program (file-name-base mathprog-ts-solver-program))
         (buffer (get-buffer-create (concat "*" program "*")))
         (args (append (list mathprog-ts-solver-program nil buffer nil)
                       mathprog-ts-solver-check-args
                       (list (file-relative-name buffer-file-name)))))
    (with-temp-buffer-window buffer nil nil
      (with-current-buffer buffer
        (buffer-disable-undo)
        (let ((status (apply #'process-file args)))
          (compilation-mode (upcase program))
          (cond ((stringp status)
                 (message "MathProg model check terminated with signal: %s"
                          status))
                ((zerop status)
                 (message "MathProg model checked successfully"))
                (t
                 (message "MathProg model check failed")
                 (setq next-error-last-buffer buffer)
                 (next-error t t))))))))


;;;; Language grammar

(defconst mathprog-ts-mode-treesit-language-source
  '(mathprog . ("https://github.com/smoeding/tree-sitter-mathprog"))
  "The language source entry for the associated MathProg language parser.
The value refers to the specific version of the parser that the mode has
been tested with.  Using this mode with either an older or more recent
version of the parser might not work as expected.")

(defun mathprog-ts-mode-install-grammar ()
  "Install the language grammar for `mathprog-ts-mode'.
The function removes existing entries for the MathProg language in
`treesit-language-source-alist' and adds the entry stored in
`mathprog-ts-mode-treesit-language-source'."
  (interactive)
  ;; Remove existing entries
  (setq treesit-language-source-alist
        (assq-delete-all 'mathprog treesit-language-source-alist))
  ;; Add the correct entry
  (add-to-list 'treesit-language-source-alist
               mathprog-ts-mode-treesit-language-source)
  ;; Install the grammar
  (treesit-install-language-grammar 'mathprog))


;;;; Major mode definition

(defvar mathprog-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; our strings
    (modify-syntax-entry ?\' "\"'"  table)
    (modify-syntax-entry ?\" "\"\"" table)
    ;; C-style comments
    (modify-syntax-entry ?/  ". 14" table)
    (modify-syntax-entry ?*  ". 23" table)
    ;; line comments
    (modify-syntax-entry ?#  "<"  table)
    (modify-syntax-entry ?\n ">#" table)
    ;; the backslash is our escape character
    (modify-syntax-entry ?\\ "\\" table)
    ;; various operators and punctuation
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?| "." table)
    table)
  "Syntax table used in `mathprog-ts-mode' buffers.")

(defvar mathprog-ts-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-v" #'mathprog-ts-check-buffer)
    map)
  "Keymap for MathProg mode buffers.")

;;;###autoload
(define-derived-mode mathprog-ts-mode prog-mode "MathProg"
  "Major mode for editing GNU MathProg files using Tree-sitter.
\\<mathprog-ts-mode-map>
The model in the current buffer can be checked by calling
`mathprog-ts-check-buffer' (bound to \\[mathprog-ts-check-buffer]).
The variable `mathprog-ts-solver-program' can be customized to choose
the executable used for checking.  Also `mathprog-ts-solver-check-args'
can be customized to set the arguments for checking the model file.

The mode needs the Tree-sitter parser for MathProg code.  A parser
suitable for the current package version can be installed using the
function `mathprog-ts-mode-install-grammar'.  Some development tools
like a C compiler are required for this.
The constant `mathprog-ts-mode-treesit-language-source' contains the
location and version of the parser that should be used for the present
release of the mode.

Fontification depends on the concrete syntax tree returned by the
Tree-sitter parser.  Syntax errors like a missing closing parenthesis or
bracket can lead to missing fontification.  This is easily resolved by
fixing the particular syntax error.

\\{mathprog-ts-mode-map}"
  (setq-local require-final-newline mode-require-final-newline)

  ;; Comments
  (setq-local comment-start      "#"
              comment-end        ""
              comment-start-skip "#+\\s-*")

  ;; Indentation
  (setq tab-width        mathprog-ts-indent-level
        indent-tabs-mode mathprog-ts-indent-tabs-mode)

  ;; Tree-sitter
  (when (treesit-ready-p 'mathprog)
    (treesit-parser-create 'mathprog)

    ;; Navigation
    (setq treesit-defun-type-regexp mathprog-ts--statements-regexp)

    ;; Font-Lock
    (setq treesit-font-lock-feature-list mathprog-ts-mode-feature-list
          treesit-font-lock-settings
          (apply #'treesit-font-lock-rules
                 mathprog-ts-mode-font-lock-settings))

    (treesit-major-mode-setup)))

(provide 'mathprog-ts-mode)

;;; mathprog-ts-mode.el ends here
