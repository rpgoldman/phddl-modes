(require 'pddl-mode)

(defvar hddl-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.HDDL\\'" . hddl-mode))
(add-to-list 'auto-mode-alist '("\\.hddl\\'" . hddl-mode))


(defvar hddl-mode-map
  (copy-keymap pddl-mode-map)
  ;; (let ((pddl-mode-map (make-sparse-keymap)))
  ;;   (define-key pddl-mode-map [return] 'newline-and-indent)
  ;;   (define-key pddl-mode-map [tab] 'indent-for-tab-command)
  ;;   (define-key pddl-mode-map '[(control c) (tab)] 'pcomplete)
  ;;   (define-key pddl-mode-map '[(control c) (\;)] 'comment-region)
  ;;   (define-key pddl-mode-map '[(control c) (control t) (a)] 'pddl-mode-template-insert-action)
  ;;   (define-key pddl-mode-map '[(control c) (control t) (p)] 'pddl-mode-template-insert-problem)
  ;;   (define-key pddl-mode-map '[(control c) (control t) (d)] 'pddl-mode-template-insert-domain)
  ;;   (define-key pddl-mode-map '[(control c) (control t) (e)] 'pddl-mode-temp)
  ;;   pddl-mode-map)
  "Keymap for HDDL major mode")

(defconst hddl-font-lock-keywords-1
  (append pddl-font-lock-keywords-1
   (list (cons (regexp-opt '(":method" ":task" ":ordered-subtasks" ":ordered-tasks" ":tasks" ":subtasks"
                             ":constants"
                             ":htn" ":order" ":ordering" ":constraints")
                           'words)
	       'font-lock-builtin-face)))
  "Minimal highlighting expressions for HDDL mode")

(defconst hddl-font-lock-keywords-2
  (append hddl-font-lock-keywords-1
	  (list (cons (regexp-opt '("define" "and" "or" "not" "problem"
				    "domain" "either" "exists" "forall"
				    "when" "assign" "scale-up" "scale-down"
                                    "imply"
				    "increase" "decrease" "start" "end" "all"
				    "at" "over" "minimize" "maximize"
				    "total-time"
                                    ;; ordering constraint
                                    "<"
                                    ;; constraint def
                                    "="
                                    )
                                  'words)
		      'font-lock-keyword-face)))
  "Additional Keywords to highlight in HDDL mode")

(defconst hddl-font-lock-keywords-3
  (append hddl-font-lock-keywords-2
	  (list (cons (regexp-opt '(":strips" ":typing" ":equality" ":adl"
				    ":negative-preconditions" ":durative-actions"
                                    ":action-costs"
				    ":disjunctive-preconditiorns" ":fluents"
				    ":existential-preconditions"
				    ":derived-predicates"
				    ":universal-preconditions"
				    ":timed-initial-literals"
                                    ;; new requirements
                                    ":hierarchy"
                                    ":htn-method-prec"
                                    )
                                  t)
		      'font-lock-constant-face)))
    "Additional Keywords to highlight in HDDL mode")
		       
(defvar hddl-font-lock-keywords hddl-font-lock-keywords-3
  "Default highlighting expressions for HDDL mode is maximum.")

(defconst hddl-mode-syntax-table
  (copy-syntax-table pddl-mode-syntax-table)
  "Syntax table for HDDL mode is the same as PDDL mode.")

(easy-menu-define hddl-mode-menu hddl-mode-map
     "Menu for HDDL mode."
     '("HDDL"
       ("Templates" ["Insert domain" pddl-mode-template-insert-domain :active t :keys "C-t d"]
	["Insert problem" pddl-mode-template-insert-problem :active t :keys "C-t p" ]
	["Insert action" pddl-mode-template-insert-action :active t :keys "C-t a"])))


(define-derived-mode hddl-mode pddl-mode "HDDL";  ;lisp-mode "PDDL"   ;
  "Major mode for editing HDDL files -- PDDL dialect for HTN planning."
  (set (make-local-variable 'font-lock-defaults) '(hddl-font-lock-keywords nil t))
  (set-syntax-table hddl-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'lisp-indent-line)
  (set (make-local-variable 'lisp-indent-function) 'hddl-indent-function)
  (easy-menu-add hddl-mode-menu hddl-mode-map)
  (pcomplete-pddl-mode-setup)
  (setq imenu-generic-expression pddl-mode-imenu-generic-expression
	imenu-case-fold-search nil
	imenu-syntax-alist pddl-mode-imenu-syntax-alist)
  (imenu-add-to-menubar "Declarations"))

;;; indentation
(put ':method 'hddl-indent-function 'defun)

(defun hddl-indent-function (indent-point state)
  "This function is a replacement for lisp-indent-function for use in HDDL mode.
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
                      (get (intern-soft function) 'hddl-indent-function)
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

(provide 'hddl-mode)
