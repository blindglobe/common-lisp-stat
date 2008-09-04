
(load "defsys")
(ls::load-stats)
(ls::use-stats)

;;; EXCL

#+excl
(progn
  (setf *default-path* "./")

  (dumplisp :name "cls"
	    :checkpoint nil
	    :restart-actions (append (copy-list *restart-actions*)
				     (list (cons :eval '(ls::ls-top-level))))))

;;; KCL

#+kcl
(progn
  (defun si:top-level ()
  (in-package 'system::user)
  (incf system::*ihs-top* 2)
  (si:error-set (ls::ls-top-level)))

  (gbc nil)
  (si:save-system "saved_kcls"))
