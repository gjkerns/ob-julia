;;; ob-julia.el --- org-babel functions for julia code evaluation

;; Copyright (C) 2013
;; Author: G. Jay Kerns, based on ob-R.el by Eric Schulte and Dan Davison

;; Org-Babel support for evaluating julia code

;;; Code:
(require 'ob)
(eval-when-compile (require 'cl))

(declare-function orgtbl-to-csv "org-table" (table params))
(declare-function julia "ext:ess-julia" (&optional start-args))
(declare-function inferior-ess-send-input "ext:ess-inf" ())
(declare-function ess-make-buffer-current "ext:ess-inf" ())
(declare-function ess-eval-buffer "ext:ess-inf" (vis))
(declare-function org-number-sequence "org-compat" (from &optional to inc))
(declare-function org-remove-if-not "org" (predicate seq))

(defconst org-babel-header-args:julia
  '((width		 . :any)
    (horizontal		 . :any)
    (results             . ((file list vector table scalar verbatim)
			    (raw org html latex code pp wrap)
			    (replace silent append prepend)
			    (output value))))
  "julia-specific header arguments.")

(add-to-list 'org-babel-tangle-lang-exts '("julia" . "jl"))

(defvar org-babel-julia-setup
  "# org-mode
import Base.display, Base.show, Base.Multimedia.showable
struct OrgEmacs <: AbstractDisplay
    outfile::String
end
display(d::OrgEmacs, x) = display(d::OrgEmacs, MIME(\"text/org\"), x)
function display(d::OrgEmacs, ::MIME\"text/org\", x)
    open(d.outfile, \"w\") do f
        show(f, MIME(\"text/org\"), x)
    end
end
# Generic fallback
show(io::IO, ::MIME\"text/org\", i) = show(io, i)
# Overload types
show(io::IO, ::MIME\"text/org\", ::Nothing) = show(io, \"\")
function show(io::IO, ::MIME\"text/org\", i::Array{T,2}) where T <: Any
    out = eachrow(i) |> x -> join([join(l, \",\") for l in x], \"\\n\")
    print(io, out)
end

function org_reload()
    \"Defines show method based on loaded packages\"
    let pkg = :DataFrames
        if isdefined(Main, pkg) && isa(getfield(Main, pkg), Module)
            @eval function show(io::IO, ::MIME\"text/org\", d::DataFrame)
                out = '|' * join(string.(names(d)), '|')
                out *= \"\\n|\" * repeat(\"-|\", length(names(d))) * '\\n'
                out *= join(['|' * join(x, '|') * '|' for x in eachrow(d) .|> collect], '\\n')
                print(io, out)
            end
        end
    end
end
")

(defcustom org-babel-julia-table-as-dict nil
  "If t, tables are imported as Dictionary, else as NamedTuple.
In both cases, if you use DataFrames you can import pass them to
`DataFrame'.
Importing NamedTuple is slower (more data) but they preserve the column order.")

(defvar org-babel-default-header-args:julia '())

(defcustom org-babel-julia-command "julia"
  "Name of command to use for executing julia code."
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defvar ess-local-process-name) ; dynamically scoped
(defun org-babel-edit-prep:julia (info)
  (let ((session (cdr (assoc :session (nth 2 info)))))
    (when (and session (string-match "^\\*\\(.+?\\)\\*$" session))
      (save-match-data (org-babel-julia-initiate-session session nil)))))

(defun org-babel-expand-body:julia (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (mapconcat
       #'identity
       ((lambda (inside) inside)
	(append (org-babel-variable-assignments:julia params)
		(list body))) "\n"))

(defun org-babel-execute:julia (body params)
  "Execute a block of julia code.
This function is called by `org-babel-execute-src-block'."
  (save-excursion
    (let* ((result-params (cdr (assoc :result-params params)))
	   (result-type (cdr (assoc :result-type params)))
	   (output-file (cdr (assoc :file params)))
           (session (org-babel-julia-initiate-session
		     (cdr (assoc :session params)) params))
	   (colnames-p (cdr (assoc :colnames params)))
	   (rownames-p (cdr (assoc :rownames params)))
	   (full-body (org-babel-expand-body:julia body params))
	   (result
	    (org-babel-julia-evaluate
	     session full-body result-type result-params
	     output-file
	     (or (equal "yes" colnames-p)
		 (org-babel-pick-name
		  (cdr (assoc :colname-names params)) colnames-p))
	     (or (equal "yes" rownames-p)
		 (org-babel-pick-name
		  (cdr (assoc :rowname-names params)) rownames-p)))))
      result)))

(defun org-babel-prep-session:julia (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (let* ((session (org-babel-julia-initiate-session session params))
	 (var-lines (org-babel-variable-assignments:julia params)))
    (org-babel-comint-in-buffer session
      (mapc (lambda (var)
              (end-of-line 1) (insert var) (comint-send-input nil t)
              (org-babel-comint-wait-for-output session))
	    var-lines))
    session))

(defun org-babel-load-session:julia (session body params)
  "Load BODY into SESSION."
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:julia session params)))
      (with-current-buffer buffer
        (goto-char (process-mark (get-buffer-process (current-buffer))))
        (insert (org-babel-chomp body)))
      buffer)))

;; helper functions
(defun org-babel-julia-assign-to-var (name value)
  "Assign `VALUE' to a variable called `NAME'."
  (format "%s = %S;" name value))

(defun org-babel-julia-assign-to-var-or-array (var)
  ""
  (if (and (listp (cdr var)) (cddr var))
      (org-babel-julia-assign-to-array (car var) (cdr var))
    (org-babel-julia-assign-to-var (car var) (cdr var))))

(defun org-babel-julia-assign-to-array (name matrix)
  "Create a Matrix (Vector{Any,2} from `MATRIX' and assign it to `NAME'"
  (format "%s = [%s];" name (mapconcat (lambda (line)
  		 (mapconcat (lambda (val)
  			      (format "%S" val))
  			    line
  			    " "))
  	       matrix
  	       ";\n ")))

(defun org-babel-julia-assign-to-dict (name column-names values)
  "Create a Dict with lists as values.
Create a Dict where keys are Symbol from `COLUMN-NAMES',
values are Array taken from `VALUES', and assign it to `NAME'"
  (format "%s = Dict(%s);" name
	  (mapconcat
	   (lambda (i)
	     (format ":%s => [%s]" (nth i column-names)
		     (mapconcat
		      (lambda (line) (format "%S" (nth i line)))
		      values
		      ",")))
	   (number-sequence 0 (1- (length column-names)))
	   ",\n")))

(defun org-babel-julia-assign-to-named-tuple (name column-names values)
  "Create a Dict with lists as values.
Create a list of NamedTuple where keys are taken from `COLUMN-NAMES',
values are taken from `VALUES', and assign it to `NAME'"
  (format "%s = [%s];" name
	  (mapconcat
	   (lambda (i)
	     (concat
	      "(" (mapconcat
		   (lambda (j)
		     (format "%s=%S"
			     (nth j column-names)
			     (nth j (nth i values))))
		   (number-sequence 0 (1- (length column-names)))
		   ",")
	      ")"))
	   (number-sequence 0 (1- (length values))) "\n")))

(defun org-babel-variable-assignments:julia (params)
  "Return list of julia statements assigning the block's variables."
  (let ((colnames (cdr (assoc :colname-names params)))
	(vars (org-babel--get-vars params)))
    (mapcar (lambda (i)
	      (let* ((var (nth i vars))
		     (column-names
		      (car (seq-filter
			    (lambda (cols)
			      (eq (car cols) (car var)))
			    colnames))))
		(if column-names
		    (if org-babel-julia-table-as-dict
			(org-babel-julia-assign-to-dict
			 (car var) (cdr column-names) (cdr var))
			(org-babel-julia-assign-to-named-tuple
			 (car var) (cdr column-names) (cdr var)))
		  (org-babel-julia-assign-to-var-or-array var))))
	    (number-sequence 0 (1- (length vars))))))

(defvar ess-ask-for-ess-directory) ; dynamically scoped

(defun org-babel-julia-initiate-session (session params)
  "If there is not a current julia process then create one."
  (unless (string= session "none")
    (let ((session (or session "*julia*"))
	  (ess-ask-for-ess-directory
	   (and (and (boundp 'ess-ask-for-ess-directory) ess-ask-for-ess-directory)
		(not (cdr (assoc :dir params))))))
      (if (org-babel-comint-buffer-livep session)
	  session
	(save-window-excursion
	  (require 'ess) (julia)
	  (rename-buffer
	   (if (bufferp session)
	       (buffer-name session)
	     (if (stringp session)
		 session
	       (buffer-name))))
	  (current-buffer))))))

(defun org-babel-julia-associate-session (session)
  "Associate julia code buffer with a julia session.
Make SESSION be the inferior ESS process associated with the
current code buffer."
  (setq ess-local-process-name
	(process-name (get-buffer-process session)))
  (ess-make-buffer-current))

(defvar org-babel-julia-eoe-indicator "print(\"org_babel_julia_eoe\")")
(defvar org-babel-julia-eoe-output "org_babel_julia_eoe")

(defvar org-babel-julia-write-object-command "org_reload();display(OrgEmacs(\"%s\"),%s)")
(defvar org-babel-julia-write-object-with-fileio-command "using FileIO;save(\"%s\",%s)")

(defun org-babel-julia-evaluate
    (session body result-type result-params output-file column-names-p row-names-p)
  "Evaluate julia code in BODY."
  (if session
      (org-babel-julia-evaluate-session
       session body result-type result-params output-file column-names-p row-names-p)
    (org-babel-julia-evaluate-external-process
     body result-type result-params output-file column-names-p row-names-p)))

(defun org-babel-julia-evaluate-external-process
    (body result-type result-params output-file column-names-p row-names-p)
  "Evaluate BODY in external julia process.
If RESULT-TYPE equals 'output then return standard output as a
string.  If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (case result-type
    (value
     (let ((tmp-file (or output-file (org-babel-temp-file "julia-"))))
       (org-babel-eval org-babel-julia-command
		       (format (if output-file
				   org-babel-julia-write-object-with-fileio-command
				 org-babel-julia-write-object-command)
			       (org-babel-process-file-name tmp-file 'noquote)
			       (format "begin\n%s\nend" body)))
       (org-babel-julia-process-value-result
	(org-babel-result-cond result-params
	  (with-temp-buffer
	    (insert-file-contents tmp-file)
	    (buffer-string))
	  (org-babel-import-elisp-from-file tmp-file '(4)))
	column-names-p)))
    (output (org-babel-eval org-babel-julia-command
			    body))))

(defun org-babel-julia-evaluate-session
    (session body result-type result-params output-file column-names-p row-names-p)
  "Evaluate BODY in SESSION.
If RESULT-TYPE equals 'output then return standard output as a
string.  If RESULT-TYPE equals 'value then return the value of the
last statement in BODY, as elisp."
  (case result-type
    (value
     (with-temp-buffer
       (insert (org-babel-chomp body))
       (let ((ess-local-process-name
	      (process-name (get-buffer-process session)))
	     (ess-eval-visibly-p nil))
	 (ess-eval-buffer nil)))
     (let ((tmp-file (or output-file (org-babel-temp-file "julia-"))))
       (org-babel-comint-eval-invisibly-and-wait-for-file
	session tmp-file
	(format (if output-file
		    org-babel-julia-write-object-with-fileio-command
		  org-babel-julia-write-object-command)
		(org-babel-process-file-name tmp-file 'noquote) "ans"))
       (org-babel-julia-process-value-result
	(org-babel-result-cond result-params
	  (with-temp-buffer
	    (insert-file-contents tmp-file)
	    (buffer-string))
	  (org-babel-import-elisp-from-file tmp-file '(4)))
	column-names-p)))
    (output
     (mapconcat
      #'org-babel-chomp
      (butlast
       (delq nil
	     (mapcar
	      (lambda (line) (when (> (length line) 0) line))
	      (mapcar
	       (lambda (line) ;; cleanup extra prompts left in output
		 (if (string-match
		      "^\\([ ]*[>+\\.][ ]?\\)+\\([[0-9]+\\|[ ]\\)" line)
		     (substring line (match-end 1))
		   line))
	       (org-babel-comint-with-output (session org-babel-julia-eoe-output)
		 (insert (mapconcat #'org-babel-chomp
				    (list body org-babel-julia-eoe-indicator)
				    "\n"))
		 (inferior-ess-send-input)))))) "\n"))))

(defun org-babel-julia-process-value-result (result column-names-p)
  "julia-specific processing of return value.
Insert hline if column names in output have been requested."
  (if column-names-p
      (cons (car result) (cons 'hline (cdr result)))
    result))

(provide 'ob-julia)

;;; ob-julia.el ends here
