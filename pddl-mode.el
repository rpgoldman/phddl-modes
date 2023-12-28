;;; pddl-mode.el --- A Planning and Domain Definition Language editing mode    -*-coding: iso-8859-1;-*-

;; Copyright (C) 2005 Surendra K Singhi
;; Copyright (c) 2010-2011 Robert P. Goldman

;; Authors: 2005      Surendra K Singhi <surendra@asu.edu>
;; Keywords: PDDL Planning files 
;; Version: 0.100
;; URL: http://www.public.asu.edu/~sksinghi/PDDL-mode.htm

;;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Purpose:
;;
;; To provide a pleasant mode to browse and edit PDDL files.
;;It provides syntax highlighting with automatic indentation,
;;templates, auto-completion, and a Declaration imenu which
;;list all the actions and the problems in the current file.
;;
;;
;; This mode supports full PDDL 2.2 
;;
;; Installation:
;; 
;; Put in your ~/.emacs:
;;      (add-to-list 'load-path "/lib/emacs/pddl-mode")
;;      (require 'pddl-mode)
;;
;; Version 0.100: PDDL-mode released
;; History:
;;
;; This mode was written by Surendra Singhi in February 2005.
;; Special thanks also goes to Stefan Monnier <monnier@iro.umontreal.ca> for
;; helping me with various parts of the code
;; If you have any problems or suggestions or patches specific to the mode
;;please contact the author via email.  
;;
;;

;;; Code:

(require 'easymenu)
(require 'font-lock)
(require 'regexp-opt)
(require 'custom)
(require 'lisp-mode)
(require 'pcomplete)
(require 'imenu)

(defvar pddl-mode-hook nil)

(defvar pddl-mode-map
  (let ((pddl-mode-map (make-sparse-keymap)))
    (define-key pddl-mode-map [return] 'newline-and-indent)
    (define-key pddl-mode-map [tab] 'indent-for-tab-command)
    (define-key pddl-mode-map '[(control c) (tab)] 'pcomplete)
    (define-key pddl-mode-map '[(control c) (\;)] 'comment-region)
    (define-key pddl-mode-map '[(control c) (control t) (a)] 'pddl-mode-template-insert-action)
    (define-key pddl-mode-map '[(control c) (control t) (p)] 'pddl-mode-template-insert-problem)
    (define-key pddl-mode-map '[(control c) (control t) (d)] 'pddl-mode-template-insert-domain)
    (define-key pddl-mode-map '[(control c) (control t) (e)] 'pddl-mode-temp)
    pddl-mode-map)
  "Keymap for PDDL major mode")

(add-to-list 'auto-mode-alist '("\\.PDDL\\'" . pddl-mode))
(add-to-list 'auto-mode-alist '("\\.pddl\\'" . pddl-mode))

(defconst pddl-font-lock-keywords-1
  (list (cons "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+.*$"
	      'font-lock-comment-face)
	(cons "\\(\\W\\|^\\)\\?\\w*" 'font-lock-variable-name-face)
	(cons (regexp-opt '(":requirements" ":types" ":constants" ":predicates"
			    ":action" ":domain" ":parameters" ":effect"
			    ":precondition" ":objects" ":init" ":goal"
			    ":functions" ":duration" ":condition" ":derived"
			    ":metric")
                          'words)
	      'font-lock-builtin-face))
  "Minimal highlighting expressions for PDDL mode")

(defconst pddl-font-lock-keywords-2
  (append pddl-font-lock-keywords-1
	  (list (cons (regexp-opt '("define" "and" "or" "not" "problem"
				    "domain" "either" "exists" "forall"
				    "when" "assign" "scale-up" "scale-down"
                                    "imply"
				    "increase" "decrease" "start" "end" "all"
				    "at" "over" "minimize" "maximize"
				    "total-time")
                                  'words)
		      'font-lock-keyword-face)))
  "Additional Keywords to highlight in PDDL mode")

(defconst pddl-font-lock-keywords-3
  (append pddl-font-lock-keywords-2
	  (list (cons (regexp-opt '(":strips" ":typing" ":equality" ":adl"
				    ":negative-preconditions" ":durative-actions"
                                    ":action-costs"
				    ":disjunctive-preconditiorns" ":fluents"
				    ":existential-preconditions"
				    ":derived-predicates"
				    ":universal-preconditions"
				    ":timed-initial-literals")
                                  t)
		      'font-lock-constant-face)))
    "Additional Keywords to highlight in PDDL mode")
		       
(defvar pddl-font-lock-keywords pddl-font-lock-keywords-3
  "Default highlighting expressions for PDDL mode is maximum.")

(defconst pddl-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?: "w" table)
    (modify-syntax-entry ?- "w" table)
    table)
  "Syntax table for PDDL mode is same as lisp mode syntax table.")

(define-skeleton pddl-mode-template-insert-action
  "Inserts the template for an action definition."
  nil
  > "(:action " (skeleton-read "action name?") \n
  > ":parameters ("_")" \n > ":precondition ()" \n > ":effect ())")

(define-skeleton pddl-mode-template-insert-domain
  "Inserts the template for a domain definition."
  nil
  > "(define (domain " (skeleton-read "domain name?") ")"\n
  > "(:requirements :strips " _ ")" \n > "(:predicates )" \n > ")")

(define-skeleton pddl-mode-template-insert-problem
  "Inserts the template for a problem definition."
  nil
  >"(define (problem " (skeleton-read "problem name?") ")"\n
  > "(:domain " (skeleton-read "domain name?") ")" \n
  > "(:init " _ ")" \n > "(:goal ))" \n)


(easy-menu-define pddl-mode-menu pddl-mode-map
     "Menu for PDDL mode."
     '("PDDL"
       ("Templates" ["Insert domain" pddl-mode-template-insert-domain :active t :keys "C-t d"]
	["Insert problem" pddl-mode-template-insert-problem :active t :keys "C-t p" ]
	["Insert action" pddl-mode-template-insert-action :active t :keys "C-t a"])))

(defvar pddl-mode-imenu-generic-expression
  '(("Actions" "(\\s-*\\:action\\s-*\\(\\s\w+\\)" 1)
    ("Problems" "(\\s-*problem\\s-*\\(\\s\w+\\)" 1)))

(defvar pddl-mode-imenu-syntax-alist '(("_-" . "w")))

(defvar pddl-mode-all-completions 
  '(":strips" ":typing" ":equality" ":adl" ":negative-preconditions" ":durative-actions"
   ":disjunctive-preconditiorns" ":fluents" ":existential-preconditions" ":derived-predicates"
   ":action-costs"
   ":universal-preconditions" ":timed-initial-literals" "define" "and" "or" "not" "problem" "imply"
   "domain" "either" "exists" "forall" "when" "assign" "scale-up" "scale-down" "increase"
   "decrease" "start" "end" "all" "at" "over" "minimize" "maximize" "total-time")
  "The list of possible completions.")

;; indentation
(put 'exists 'pddl-indent-function 'defun)
(put 'forall 'pddl-indent-function 'defun)
(put ':action 'pddl-indent-function 'defun)
(put 'define 'pddl-indent-function 'defun)



(defun pcomplete-pddl-mode-setup ()
  "Function to setup the pcomplete variables."
 (interactive)
  (set (make-local-variable 'pcomplete-parse-arguments-function)
       'pcomplete-parse-pddl-mode-arguments)
  (set (make-local-variable 'pcomplete-default-completion-function)
       'pcomplete-pddl-mode-default-completion))

(defun pcomplete-pddl-mode-default-completion()
  (pcomplete-here pddl-mode-all-completions))

(defun pcomplete-parse-pddl-mode-arguments ()
  (save-excursion
    (let* ((thispt (point))
	   (pt (search-backward-regexp "[ \t\n]" nil t))
	   (ptt (if pt (+ pt 1) thispt)))
      (list
       (list "dummy" (buffer-substring-no-properties ptt thispt))
       (point-min) ptt))))


;;; changing to derive this from no other mode --- deriving it from
;;; common-lisp mode as before made it make all kinds of inappropriate
;;; generalizations about the meaninds of its constructs. [2011/09/02:rpg]
(define-derived-mode pddl-mode nil "PDDL";  ;lisp-mode "PDDL"   ;
  "Major mode for editing PDDL files"
  (set (make-local-variable 'comment-start) ";")
  ;everything that begins with ';' is comment and remember \; is not a comment but \\; is
  (set (make-local-variable 'comment-start-skip) "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+ *")
  (set (make-local-variable 'font-lock-defaults) '(pddl-font-lock-keywords nil t))
  (set-syntax-table pddl-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (set (make-local-variable 'lisp-indent-function) 'pddl-indent-function)
  (easy-menu-add pddl-mode-menu pddl-mode-map)
  (pcomplete-pddl-mode-setup)
  (setq imenu-generic-expression pddl-mode-imenu-generic-expression
	imenu-case-fold-search nil
	imenu-syntax-alist pddl-mode-imenu-syntax-alist)
  (imenu-add-to-menubar "Declarations"))

(defun pddl-indent-function (indent-point state)
  "This function is a replacement for lisp-indent-function for use in PDDL mode.
See the documentation string for that function for more information.

  The critical difference is the property used to compute special
indentation:

If the current line is in a call to a Lisp function that has a non-nil
property `pddl-indent-function' it specifies how to indent.  
The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
		(progn (goto-char calculate-lisp-indent-last-sexp)
		       (beginning-of-line)
		       (parse-partial-sexp (point)
					   calculate-lisp-indent-last-sexp 0 t)))
	    ;; Indent under the list or under the first sexp on the same
	    ;; line as calculate-lisp-indent-last-sexp.  Note that first
	    ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or
                      (get (intern-soft function) 'pddl-indent-function)
                      (get (intern-soft function) 'lisp-indent-function)
                      (get (intern-soft function) 'lisp-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method indent-point state)))))))

(provide 'pddl-mode)

;;; pddl-mode.el ends here
