
(require 'ess-site)

(setq  inferior-julia-program-name "/path/to/julia-release-basic")

(require 'org)

(setq org-confirm-babel-evaluate nil)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)   
(add-hook 'org-mode-hook 'org-display-inline-images)

(load "/path/to/ob-julia.el")

(add-to-list 'load-path "/path/to/ob-julia.el")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) (julia . t)))

(require 'ox-html)

(require 'ox-latex)

(require 'ox-beamer)

(add-to-list 'org-latex-classes
	     '("beamer"
	       "\\documentclass[presentation]{beamer}
        \[DEFAULT-PACKAGES]
        \[PACKAGES]
        \[EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
