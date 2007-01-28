;;;;
;;;; statistics.lsp XLISP-STAT statistics functions
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(provide "statistics")

#+:CLtL2
(in-package lisp-stat)
#-:CLtL2
(in-package 'lisp-stat)

(export '(open-file-dialog read-data-file read-data-columns load-data 
	  load-example *variables* *ask-on-redefine* def variables savevar
	  undef standard-deviation quantile median interquartile-range
	  fivnum covariance-matrix difference rseq matrix print-matrix solve
	  backsolve eigenvalues eigenvectors accumulate cumsum combine
	  lowess))

(import 'ls-basics::|base-lowess|)

;;;;
;;;; Data File Reading 
;;;;

(defun count-file-columns (fname)
"Args: (fname)
Returns the number of lisp items on the first nonblank line of file FNAME."
  (with-open-file (f fname)
    (if f
        (let ((line (do ((line (read-line f) (read-line f))) 
                        ((or (null line) (< 0 (length line))) line))))
          (if line
              (with-input-from-string (s line)
                (do ((n 0 (+ n 1)) (eof (gensym))) 
                    ((eq eof (read s nil eof)) n))))))))

#+xlisp (defvar *xlisptable* *readtable*)

(if (not (fboundp 'open-file-dialog))
  #+dialogs
  (defun open-file-dialog (&optional set)
    (get-string-dialog "Enter a data file name:"))
  #-dialogs
  (defun open-file-dialog (&optional set)
    (error "You must provide a file name explicitly")))

(defun read-data-file (&optional (file (open-file-dialog t)))
"Args:  (file)
Returns a list of all lisp objects in FILE. FILE can be a string or a symbol,
in which case the symbol'f print name is used."
  (if file
      (let ((eof (gensym)))
        (with-open-file (f file)
          (if f
	      (do* ((r (read f nil eof) (read f nil eof))
		    (x (list nil))
		    (tail x (cdr tail)))
		   ((eq r eof) (cdr x))
		   (setf (cdr tail) (list r))))))))

;;; New definition to avoid stack size limit in apply
(defun read-data-columns (&optional (file (open-file-dialog t))
                                    (cols (if file 
                                              (count-file-columns file))))
"Args: (&optional file cols)
Reads the data in FILE as COLS columns and returns a list of lists representing the columns."
  (if (and file cols)
      (transpose (split-list (read-data-file file) cols))))

#+unix
(defun load-data (file)
"Args: (file)
Read in data file from the data examples library."
  (if (load (format nil "~aData/~a" *default-path* file))
      t
      (load (format nil "~aExamples/~a" *default-path* file))))

#+unix
(defun load-example (file)
"Args: (file)
Read in lisp example file from the examples library."
  (if (load (format nil "~aExamples/~a" *default-path* file))
      t
      (load (format nil "~aData/~a" *default-path* file))))
#+macintosh
(defun load-data (s) (require s (concatenate 'string ":Data:" s)))
#+macintosh
(defun load-example (s) (require s (concatenate 'string ":Examples:" s)))

#+msdos
(defun load-data (file)
"Args: (file)
Read in data file from the data examples library."
  (load (format nil "~aData\\~a" *default-path* file)))

#+msdos
(defun load-example (file)
"Args: (file)
Read in lisp example file from the examples library."
  (load (format nil "~aExamples\\~a" *default-path* file)))

;;;;
;;;; Listing and Saving Variables and Functions
;;;;

(defvar *variables* nil)
(defvar *ask-on-redefine* nil)

(defmacro def (symbol value)
"Syntax: (def var form)
VAR is not evaluated and must be a symbol.  Assigns the value of FORM to
VAR and adds VAR to the list *VARIABLES* of def'ed variables. Returns VAR.
If VAR is already bound and the global variable *ASK-ON-REDEFINE*
is not nil then you are asked if you want to redefine the variable."
  `(unless (and *ask-on-redefine*
                (boundp ',symbol)
                (not (y-or-n-p "Variable has a value. Redefine?")))
           (pushnew ',symbol *variables*)
           (setf ,symbol ,value)
           ',symbol))
  
(defun variables-list () 
    (mapcar #'intern (sort-data (mapcar #'string *variables*))))

(defun variables ()
"Args:()
Returns a list of the names of all def'ed variables to STREAM"
  (if *variables*
      (mapcar #'intern (sort-data (mapcar #'string *variables*)))))
  
(defun savevar (vars file)
"Args: (vars file-name-root)
VARS is a symbol or a list of symbols. FILE-NAME-ROOT is a string (or a symbol
whose print name is used) not endinf in .lsp. The VARS and their current values
are written to the file FILE-NAME-ROOT.lsp in a form suitable for use with the
load command."
  (with-open-file (f (strcat (string file) ".lsp") :direction :output)
    (let ((vars (if (consp vars) vars (list vars))))
      (flet ((save-one (x)
	       (let ((v (symbol-value x)))
		 (if (objectp v) 
		     (format f "(def ~s ~s)~%" x (send v :save))
		   (format f "(def ~s '~s)~%" x v)))))
	    (mapcar #'save-one vars))
      vars)))

(defun undef (v)
"Args: (v)
If V is the symbol of a defined variable the variable it is unbound and
removed from the list of defined variables. If V is a list of variable
names each is unbound and removed. Returns V."
  (dolist (s (if (listp v) v (list v)))
          (when (member s *variables*)
                (setq *variables* (delete s *variables*))
                (makunbound s)))
  v)
        

;;;;
;;;; Basic Summary Statistics
;;;;

(defun standard-deviation (x)
"Args: (x)
Returns the standard deviation of the elements x. Vector reducing."
  (let ((n (count-elements x))
        (r (- x (mean x))))
    (sqrt (* (mean (* r r)) (/ n (- n 1))))))

(defun quantile (x p)
"Args: (x p)
Returns the P-th quantile(s) of sequence X. P can be a number or a sequence."
  (let* ((x (sort-data x))
         (n (length x))
         (np (* p (- n 1)))
         (low (floor np))
         (high (ceiling np)))
    (/ (+ (select x low) (select x high)) 2)))
    
(defun median (x) 
"Args: (x)
Returns the median of the elements of X."
  (quantile x 0.5))

(defun interquartile-range (x) 
"Args: (number-data)
Returns the interquartile range of the elements of X."
  (apply #'- (quantile x '(0.75 0.25))))

(defun fivnum (x) 
"Args: (number-data)
Returns the five number summary (min, 1st quartile, medinan, 3rd quartile,
max) of the elements X."
  (quantile x '(0 .25 .5 .75 1)))

(defun covariance-matrix (&rest args)
"Args: (&rest args)
Returns the sample covariance matrix of the data columns in ARGS. ARGS may
consist of lists, vectors or matrices."
  (let ((columns (apply #'append 
                        (mapcar #'(lambda (x) 
				    (if (matrixp x) (column-list x) (list x)))
                                args))))
    (/ (cross-product (apply #'bind-columns 
                             (- columns (mapcar #'mean columns))))
       (- (length (car columns)) 1))))

;;;;
;;;; Basic Sequence Operations
;;;;

(defun difference (x)
"Args: (x)
Returns differences for a sequence X."
  (let ((n (length x)))
    (- (select x (iseq 1 (1- n))) (select x (iseq 0 (- n 2))))))

(defun rseq (a b num)
"Args: (a b num)
Returns a list of NUM equally spaced points starting at A and ending at B."
  (+ a (* (iseq 0 (1- num)) (/ (float (- b a)) (1- num)))))


;;;;
;;;; Linear Algebra Functions
;;;;

(defun matrix (dim data)
"Args: (dim data)
returns a matrix of dimensions DIM initialized using sequence DATA
in row major order." 
  (let ((dim (coerce dim 'list))
        (data (coerce data 'list)))
    (make-array dim :initial-contents (split-list data (nth 1 dim)))))

(defun print-matrix (a &optional (stream *standard-output*))
"Args: (matrix &optional stream)
Prints MATRIX to STREAM in a nice form that is still machine readable"
  (unless (matrixp a) (error "not a matrix - ~a" a))
  (let ((size (min 15 (max (map-elements #'flatsize a)))))
    (format stream "#2a(~%")
    (dolist (x (row-list a))
            (format stream "    (")
            (let ((n (length x)))
              (dotimes (i n)
                       (let ((y (aref x i)))
                         (cond
                           ((integerp y) (format stream "~vd" size y))
                           ((floatp y) (format stream "~vg" size y))
                           (t (format stream "~va" size y))))
                       (if (< i (- n 1)) (format stream " "))))
            (format stream ")~%"))
    (format stream "   )~%")
    nil))

(defun solve (a b)
"Args: (a b)
Solves A x = B using LU decomposition and backsolving. B can be a sequence
or a matrix."
  (let ((lu (lu-decomp a)))
    (if (matrixp b)
        (apply #'bind-columns 
               (mapcar #'(lambda (x) (lu-solve lu x)) (column-list b)))
        (lu-solve lu b))))
        
(defun backsolve (a b)
"Args: (a b)
Solves A x = B by backsolving, assuming A is upper triangular. B must be a
sequence. For use with qr-decomp."
  (let* ((n (length b))
         (sol (make-array n)))
    (dotimes (i n)
             (let* ((k (- n i 1))
                    (val (elt b k)))
               (dotimes (j i)
                        (let ((l (- n j 1)))
                          (setq val (- val (* (aref sol l) (aref a k l))))))
               (setf (aref sol k) (/ val (aref a k k)))))
    (if (listp b) (coerce sol 'list) sol)))

(defun eigenvalues (a) 
"Args: (a)
Returns list of eigenvalues of square, symmetric matrix A"
  (first (eigen a)))

(defun eigenvectors (a) 
"Args: (a)
Returns list of eigenvectors of square, symmetric matrix A"
  (second (eigen a)))

(defun accumulate (f s)
"Args: (f s)
Accumulates elements of sequence S using binary function F.
(accumulate #'+ x) returns the cumulative sum of x."
  (let* ((result (list (elt s 0)))
         (tail result))
    (flet ((acc (dummy x)
                (rplacd tail (list (funcall f (first tail) x)))
                (setf tail (cdr tail))))
      (reduce #'acc s))
    (if (vectorp s) (coerce result 'vector) result)))

(defun cumsum (x)
"Args: (x)
Returns the cumulative sum of X."
  (accumulate #'+ x))

(defun combine (&rest args) 
"Args (&rest args) 
Returns sequence of elements of all arguments."
  (copy-seq (element-seq args)))

(defun lowess (x y &key (f .25) (steps 2) (delta -1) sorted)
"Args: (x y &key (f .25) (steps 2) delta sorted)
Returns (list X YS) with YS the LOWESS fit. F is the fraction of data used for
each point, STEPS is the number of robust iterations. Fits for points within
DELTA of each other are interpolated linearly. If the X values setting SORTED
to T speeds up the computation."
  (let ((x (if sorted x (sort-data x)))
        (y (if sorted y (select y (order x))))
        (delta (if (> delta 0.0) delta (/ (- (max x) (min x)) 50))))
    (list x (|base-lowess| x y f steps delta))))
