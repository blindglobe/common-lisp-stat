;;;; linalg -- Lisp-Stat interface to basic linear algebra routines.
;;;; 
;;;; Copyright (c) 1991, by Luke Tierney. Permission is granted for
;;;; unrestricted use.

(provide "linalg")

;;;;
;;;; Package Setup
;;;;

#+:CLtL2
(in-package lisp-stat-basics)
#-:CLtL2
(in-package 'lisp-stat-basics)

(export '(chol-decomp lu-decomp lu-solve determinant inverse sv-decomp
	  qr-decomp rcondest make-rotation spline kernel-dens kernel-smooth
	  fft make-sweep-matrix sweep-operator ax+y numgrad numhess
	  split-list eigen))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;                 Lisp to C number conversion and checking
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; Lisp to/from C sequence and matrix conversion and checking
;;;;

(defun is-cons (a) (if (consp a) 1 0))

(defun check-fixnum (a)
  (if (/= 0 (la-data-mode a)) (error "not an integer sequence - ~s" a)))

(defun check-real (data)
  (let ((data (compound-data-seq data)))
    (cond 
     ((vectorp data)
      (let ((n (length data)))
	(declare (fixnum n))
	(dotimes (i n)
		 (declare (fixnum i))
		 (check-one-real (aref data i)))))
     ((consp data) (dolist (x data) (check-one-real x)))
     (t (error "bad sequence - ~s" data)))))

(defun vec-assign (a i x) (setf (aref a i) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;             Lisp Interfaces to Linear Algebra Routines
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;; Cholesky Decomposition
;;;;

(defun chol-decomp (a &optional (maxoffl 0.0))
"Args: (a)
Modified Cholesky decomposition. A should be a square, symmetric matrix.
Computes lower triangular matrix L such that L L^T = A + D where D is a
diagonal matrix. If A is strictly positive definite D will be zero.
Otherwise, D is as small as possible to make A + D numerically strictly
positive definite. Returns a list (L (max D))."
  (check-square-matrix a)
  (check-real a)
  (let* ((n (array-dimension a 0))
	 (result (make-array (list n n)))
	 (dpars (list maxoffl 0.0)))
    (check-real dpars)
    (let ((mat (la-data-to-matrix a mode-re))
	  (dp (la-data-to-vector dpars mode-re)))
      (unwind-protect
	  (progn
	    (chol-decomp-front mat n dp)
	    (la-matrix-to-data mat n n mode-re result)
	    (la-vector-to-data dp 2 mode-re dpars))
	(la-free-matrix mat n)
	(la-free-vector dp)))
    (list result (second dpars))))

;;;;
;;;; LU Decomposition
;;;;

(defun lu-decomp (a)
"Args: (a)
A is a square matrix of numbers (real or complex). Computes the LU
decomposition of A and returns a list of the form (LU IV D FLAG), where
LU is a matrix with the L part in the lower triangle, the U part in the 
upper triangle (the diagonal entries of L are taken to be 1), IV is a vector
describing the row permutation used, D is 1 if the number of permutations
is odd, -1 if even, and FLAG is T if A is numerically singular, NIL otherwise.
Used bu LU-SOLVE."
  (check-square-matrix a)
  (let* ((n (array-dimension a 0))
	 (mode (max mode-re (la-data-mode a)))
	 (result (list (make-array (list n n)) (make-array n) nil nil)))
    (let ((mat (la-data-to-matrix a mode))
	  (iv (la-vector n mode-in))
	  (d (la-vector 1 mode-re))
	  (singular 0))
      (unwind-protect
	  (progn
	    (setf singular (lu-decomp-front mat n iv mode d))
	    (la-matrix-to-data mat n n mode (first result))
	    (la-vector-to-data iv n mode-in (second result))
	    (setf (third result) (la-get-double d 0))
	    (setf (fourth result) (if (= singular 0) nil t)))
	(la-free-matrix mat n)
	(la-free-vector iv)
	(la-free-vector d)))
    result))

(defun lu-solve (lu lb)
"Args: (lu b)
LU is the result of (LU-DECOMP A) for a square matrix A, B is a sequence.
Returns the solution to the equation Ax = B. Signals an error if A is
singular."
  (let ((la (first lu))
	(lidx (second lu)))
    (check-square-matrix la)
    (check-sequence lidx)
    (check-sequence lb)
    (check-fixnum lidx)
    (let* ((n (num-rows la))
	   (result (make-sequence (if (consp lb) 'list 'vector) n))
	   (a-mode (la-data-mode la))
	   (b-mode (la-data-mode lb)))
      (if (/= n (length lidx)) (error "index sequence is wrong length"))
      (if (/= n (length lb)) (error "right hand side is wrong length"))
      (let* ((mode (max mode-re a-mode b-mode))
	     (a (la-data-to-matrix la mode))
	     (indx (la-data-to-vector lidx mode-in))
	     (b (la-data-to-vector lb mode))
	     (singular 0))
	(unwind-protect
	    (progn
	      (setf singular (lu-solve-front a n indx b mode))
	      (la-vector-to-data b n mode result))
	  (la-free-matrix a n)
	  (la-free-vector indx)
	  (la-free-vector b))
	(if (/= 0 singular) (error "matrix is (numerically) singular"))
	result))))

(defun determinant (a)
"Args: (m)
Returns the determinant of the square matrix M."
  (let* ((lu (lu-decomp a))
	 (la (first lu))
	 (n (num-rows a))
	 (d1 (third lu))
	 (d2 0.d0))
    (declare (fixnum n))
    (flet ((fabs (x) (float (abs x) 0.d0)))
      (dotimes (i n (* d1 (exp d2)))
	(declare (fixnum i))
        (let* ((x (aref la i i))
	       (magn (fabs x)))
          (if (= 0.0 magn) (return 0.d0))
	  (setf d1 (* d1 (/ x magn)))
	  (setf d2 (+ d2 (log magn))))))))
          
(defun inverse (a)
"Args: (m)
Returns the inverse of the the square matrix M; signals an error if M is ill
conditioned or singular"
  (check-square-matrix a)
  (let ((n (num-rows a))
	(mode (max mode-re (la-data-mode a))))
    (declare (fixnum n))
    (let ((result (make-array (list n n) :initial-element 0)))
      (dotimes (i n)
	       (declare (fixnum i))
	       (setf (aref result i i) 1))
      (let ((mat (la-data-to-matrix a mode))
	    (inv (la-data-to-matrix result mode))
	    (iv (la-vector n mode-in))
	    (v (la-vector n mode))
	    (singular 0))
	(unwind-protect
	    (progn
	      (setf singular (lu-inverse-front mat n iv v mode inv))
	      (la-matrix-to-data inv n n mode result))
	  (la-free-matrix mat n)
	  (la-free-matrix inv n)
	  (la-free-vector iv)
	  (la-free-vector v))
	(if (/= singular 0) (error "matrix is (numerically) singular"))
	result))))

;;;;
;;;; SV Decomposition
;;;;

(defun sv-decomp (a)
"Args: (a)
A is a matrix of real numbers with at least as many rows as columns.
Computes the singular value decomposition of A and returns a list of the form
(U W V FLAG) where U and V are matrices whose columns are the left and right
singular vectors of A and W is the sequence of singular values of A. FLAG is T
if the algorithm converged, NIL otherwise."
  (check-matrix a)
  (let* ((m (num-rows a))
	 (n (num-cols a))
	 (mode (max mode-re (la-data-mode a)))
	 (result (list (make-array (list m n)) 
		       (make-array n)
		       (make-array (list n n))
		       nil)))
    (if (< m n) (error "number of rows less than number of columns"))
    (if (= mode mode-cx) (error "complex SVD not available yet"))
    (let ((mat (la-data-to-matrix a mode))
	  (w (la-vector n mode-re))
	  (v (la-matrix n n mode-re))
	  (converged 0))
      (unwind-protect
	  (progn
	    (setf converged (sv-decomp-front mat m n w v))
	    (la-matrix-to-data mat m n mode (first result))
	    (la-vector-to-data w n mode (second result))
	    (la-matrix-to-data v n n mode (third result))
	    (setf (fourth result) (if (/= 0 converged) t nil)))
	(la-free-matrix mat m)
	(la-free-vector w)
	(la-free-matrix v n))
      result)))


;;;;
;;;; QR Decomposition
;;;;

(defun qr-decomp (a &optional pivot)
"Args: (a &optional pivot)
A is a matrix of real numbers with at least as many rows as columns. Computes
the QR factorization of A and returns the result in a list of the form (Q R).
If PIVOT is true the columns of X are first permuted to place insure the
absolute values of the diagonal elements of R are nonincreasing. In this case
the result includes a third element, a list of the indices of the columns in
the order in which they were used."
  (check-matrix a)
  (let* ((m (num-rows a))
	 (n (num-cols a))
	 (mode (max mode-re (la-data-mode a)))
	 (p (if pivot 1 0))
	 (result (if pivot
		     (list (make-array (list m n))
			   (make-array (list n n))
			   (make-array n))
		     (list (make-array (list m n)) (make-array (list n n))))))
    (if (< m n) (error "number of rows less than number of columns"))
    (if (= mode mode-cx) (error "complex QR decomposition not available yet"))
    (let ((mat (la-data-to-matrix a mode))
	  (v (la-matrix n n mode-re))
	  (jpvt (la-vector n mode-in)))
      (unwind-protect
	  (progn
	    (qr-decomp-front mat m n v jpvt p)
	    (la-matrix-to-data mat m n mode (first result))
	    (la-matrix-to-data v n n mode (second result))
	    (if pivot (la-vector-to-data jpvt n mode-in (third result))))
	(la-free-matrix mat m)
	(la-free-matrix v n)
	(la-free-vector jpvt))
      result)))

;;;;
;;;; Estimate of Condition Number for Lower Triangular Matrix
;;;;

(defun rcondest (a)
"Args: (a)
Returns an estimate of the reciprocal of the L1 condition number of an upper
triangular matrix a."
  (check-square-matrix a)
  (let ((mode (max mode-re (la-data-mode a)))
	(n (num-rows a)))
    (if (= mode mode-cx)
	(error "complex condition estimate not available yet"))
    (let ((mat (la-data-to-matrix a mode))
	  (est 0.0))
      (unwind-protect
	  (setf est (rcondest-front mat n))
	(la-free-matrix mat n))
      est)))
  
;;;;
;;;; Make Rotation Matrix
;;;;

(defun make-rotation (x y &optional alpha)
"Args: (x y &optional alpha)
Returns a rotation matrix for rotating from X to Y, or from X toward Y 
by angle ALPHA, in radians. X and Y are sequences of the same length."
  (check-sequence x)
  (check-sequence y)
  (if alpha (check-one-real alpha))
  (let* ((n (length x))
	 (mode (max mode-re (la-data-mode x) (la-data-mode y)))
	 (use-angle (if alpha 1 0))
	 (angle (if alpha (float alpha 0.0) 0.0))
	 (result (make-array (list n n))))
    (if (/= n (length y)) (error "sequences not the same length"))
    (if (= mode mode-cx) (error "complex data not supported yet"))
    (let ((px (la-data-to-vector x mode-re))
	  (py (la-data-to-vector y mode-re))
	  (rot (la-matrix n n mode-re)))
      (unwind-protect
	  (progn
	    (make-rotation-front n rot px py use-angle angle)
	    (la-matrix-to-data rot n n mode-re result))
	(la-free-vector px)
	(la-free-vector py)
	(la-free-matrix rot n))
      result)))

;;;;
;;;; Eigenvalues and Vectors
;;;;

(defun eigen (a)
"Args: (a)
Returns list of list of eigenvalues and list of eigenvectors of square,
symmetric matrix A. Third element of result is NIL if algorithm converges.
If the algorithm does not converge, the third element is an integer I.
In this case the eigenvalues 0, ..., I are not reliable."
  (check-square-matrix a)
  (let ((mode (max mode-re (la-data-mode a)))
	(n (num-rows a)))
    (if (= mode mode-cx) (error "matrix must be real and symmetric"))
    (let ((evals (make-array n))
	  (evecs (make-list (* n n)))
	  (pa (la-data-to-vector (compound-data-seq a) mode-re))
	  (w (la-vector n mode-re))
	  (z (la-vector (* n n) mode-re))
	  (fv1 (la-vector n mode-re))
	  (ierr 0))
      (unwind-protect
	  (progn
	    (setf ierr (eigen-front pa n w z fv1))
	    (la-vector-to-data w n mode-re evals)
	    (la-vector-to-data z (* n n) mode-re evecs))
	(la-free-vector pa)
	(la-free-vector z)
	(la-free-vector w)
	(la-free-vector fv1))
      (list (nreverse evals)
	    (nreverse (mapcar #'(lambda (x) (coerce x 'vector))
			      (split-list evecs n)))
	    (if (/= 0 ierr) (- n ierr))))))

;;;;
;;;; Spline Interpolation
;;;;

(defun make-smoother-args (x y xvals)
  (check-sequence x)
  (check-real x)
  (when y
	(check-sequence y)
	(check-real y))
  (unless (integerp xvals)
	  (check-sequence xvals)
	  (check-real xvals))
  (let* ((n (length x))
	 (ns (if (integerp xvals) xvals (length xvals)))
	 (result (list (make-list ns) (make-list ns))))
    (if (and y (/= n (length y))) (error "sequences not the same length"))
    (list x y n (if (integerp xvals) 0 1) ns xvals result)))

(defun get-smoother-result (args) (seventh args))

(defmacro with-smoother-data ((x y xvals is-reg) &rest body)
  `(progn 
     (check-sequence ,x)
     (check-real ,x)
     (when ,is-reg
	   (check-sequence ,y)
	   (check-real ,y))
     (unless (integerp ,xvals)
	     (check-sequence ,xvals)
	     (check-real ,xvals))
     (let* ((supplied (not (integerp ,xvals)))
	    (n (length ,x))
	    (ns (if supplied (length ,xvals) ,xvals))
	    (result (list (make-list ns) (make-list ns))))
       (if (and ,is-reg (/= n (length ,y)))
	   (error "sequences not the same length"))
       (if (and (not supplied) (< ns 2))
	   (error "too few points for interpolation"))
       (let* ((px (la-data-to-vector ,x mode-re))
	      (py (if ,is-reg (la-data-to-vector ,y mode-re)))
	      (pxs (if supplied 
		       (la-data-to-vector ,xvals mode-re)
		       (la-vector ns mode-re)))
	      (pys (la-vector ns mode-re)))
	 (unless supplied (la-range-to-rseq n px ns pxs))
	 (unwind-protect
	     (progn ,@body
		    (la-vector-to-data pxs ns mode-re (first result))
		    (la-vector-to-data pys ns mode-re (second result)))
	   (la-free-vector px)
	   (if ,is-reg (la-free-vector py))
	   (la-free-vector pxs)
	   (la-free-vector pys))
	 result))))

(defun spline (x y &key (xvals 30))
"Args: (x y &key xvals)
Returns list of x and y values of natural cubic spline interpolation of (X,Y).
X must be strictly increasing. XVALS can be an integer, the number of equally
spaced points to use in the range of X, or it can be a sequence of points at 
which to interpolate."
  (with-smoother-data (x y xvals t)
    (let ((work (la-vector (* 2 n) mode-re))
	  (error 0))
      (unwind-protect
	  (setf error (spline-front n px py ns pxs pys work))
	(la-free-vector work))
      (if (/= error 0) (error "bad data for splines")))))

;;;;
;;;; Kernel Density Estimators and Smoothers
;;;;

(defun kernel-type-code (type)
  (cond ((eq type 'u) 0)
	((eq type 't) 1)
	((eq type 'g) 2)
	(t 3)))

(defun kernel-dens (x &key (type 'b) (width -1.0) (xvals 30))
"Args: (x &key xvals width type)
Returns list of x and y values of kernel density estimate of X. XVALS can be an
integer, the number of equally spaced points to use in the range of X, or it
can be a sequence of points at which to interpolate. WIDTH specifies the
window width. TYPE specifies the lernel and should be one of the symbols G, T,
U or B for gaussian, triangular, uniform or bisquare. The default is B."
  (check-one-real width)
  (with-smoother-data (x nil xvals nil)
    (let ((code (kernel-type-code type))
	  (error 0))
      (setf error (kernel-dens-front px n width pxs pys ns code))
      (if (/= 0 error) (error "bad kernel density data")))))

(defun kernel-smooth (x y &key (type 'b) (width -1.0) (xvals 30))
"Args: (x y &key xvals width type)
Returns list of x and y values of kernel smooth of (X,Y). XVALS can be an
integer, the number of equally spaced points to use in the range of X, or it
can be a sequence of points at which to interpolate. WIDTH specifies the
window width. TYPE specifies the lernel and should be one of the symbols G, T,
U or B for Gaussian, triangular, uniform or bisquare. The default is B."
  (check-one-real width)
  (with-smoother-data (x y xvals t)
    (let ((code (kernel-type-code type))
          (error 0))
      (kernel-smooth-front px py n width pxs pys ns code)
      (if (/= 0 error) (error "bad kernel density data")))))

;;;;
;;;; Lowess Smoother Interface
;;;;

(defun |base-lowess| (s1 s2 f nsteps delta)
  (check-sequence s1)
  (check-sequence s2)
  (check-real s1)
  (check-real s2)
  (check-one-real f)
  (check-one-fixnum nsteps)
  (check-one-real delta)
  (let* ((n (length s1))
	 (result (make-list n)))
    (if (/= n (length s2)) (error "sequences not the same length"))
    (let ((x (la-data-to-vector s1 mode-re))
	  (y (la-data-to-vector s2 mode-re))
	  (ys (la-vector n mode-re))
	  (rw (la-vector n mode-re))
	  (res (la-vector n mode-re))
	  (error 0))
      (unwind-protect
	  (progn
	    (setf error (base-lowess-front x y n f nsteps delta ys rw res))
	    (la-vector-to-data ys n mode-re result))
	(la-free-vector x)
	(la-free-vector y)
	(la-free-vector ys)
	(la-free-vector rw)
	(la-free-vector res))
      (if (/= error 0) (error "bad data for lowess"))
      result)))

#|
static LVAL add_contour_point(i, j, k, l, x, y, z, v, result)
	int i, j, k, l;
	RVector x, y;
	RMatrix z;
	double v;
	LVAL result;
{
  LVAL pt;
  double p, q;
  
  if ((z[i][j] <= v && v < z[k][l]) || (z[k][l] <= v && v < z[i][j])) {
    xlsave(pt);
    pt = mklist(2, NIL);
    p = (v - z[i][j]) / (z[k][l] - z[i][j]);
    q = 1.0 - p;
    rplaca(pt, cvflonum((FLOTYPE) (q * x[i] + p * x[k])));
    rplaca(cdr(pt), cvflonum((FLOTYPE) (q * y[j] + p * y[l])));
    result = cons(pt, result);
    xlpop();
  }
  return(result);
}

LVAL xssurface_contour()
{
  LVAL s1, s2, mat, result;
  RVector x, y;
  RMatrix z;
  double v;
  int i, j, n, m;
  
  s1 = xsgetsequence();
  s2 = xsgetsequence();
  mat = xsgetmatrix();
  v = makedouble(xlgetarg());
  xllastarg();
    
  n = seqlen(s1); m = seqlen(s2);
  if (n != numrows(mat) || m != numcols(mat)) xlfail("dimensions do not match");
  if (data_mode(s1) == CX || data_mode(s2) == CX || data_mode(mat) == CX)
    xlfail("data must be real");
  
  x = (RVector) data_to_vector(s1, RE);
  y = (RVector) data_to_vector(s2, RE);
  z = (RMatrix) data_to_matrix(mat, RE);
  
  xlsave1(result);
  result = NIL;
  for (i = 0; i < n - 1; i++) {
  	for (j = 0; j < m - 1; j++) {
  	  result = add_contour_point(i, j, i, j+1, x, y, z, v, result);
  	  result = add_contour_point(i, j+1, i+1, j+1, x, y, z, v, result);
  	  result = add_contour_point(i+1, j+1, i+1, j, x, y, z, v, result);
  	  result = add_contour_point(i+1, j, i, j, x, y, z, v, result);
  	}
  }
  xlpop();
  
  free_vector(x);
  free_vector(y);
  free_matrix(z, n);
  
  return(result);
}
|#

;;;;
;;;; FFT
;;;;

(defun fft (x &optional inverse)
"Args: (x &optional inverse)
Returns unnormalized Fourier transform of X, or inverse transform if INVERSE
is true."
  (check-sequence x)
  (let* ((n (length x))
	 (mode (la-data-mode x))
	 (isign (if inverse -1 1))
	 (result (if (consp x) (make-list n) (make-array n))))
    (let ((px (la-data-to-vector x mode-cx))
	  (work (la-vector (+ (* 4 n) 15) mode-re)))
      (unwind-protect
	  (progn
	    (fft-front n px work isign)
	    (la-vector-to-data px n mode-cx result))
	(la-free-vector px)
	(la-free-vector work))
      result)))

;;;;
;;;; SWEEP Operator
;;;;

(defun make-sweep-front (x y w n p mode has_w x_mean result)
  (declare (fixnum n p mode has_w))
  (let ((x_data nil)
	(result_data nil)
	(val 0.0)
	(dxi 0.0)
	(dyi 0.0)
	(dv 0.0)
	(dw 0.0)
	(sum_w 0.0)
	(dxik 0.0)
	(dxjk 0.0)
	(dyj 0.0)
	(dx_meani 0.0)
	(dx_meanj 0.0)
	(dy_mean 0.0)
	(has-w (if (/= 0 has_w) t nil))
	(RE 1))
    (declare-double val dxi dyi dv dw sum_w dxik dxjk dyj
		    dx_meani dx_meanj dy_mean)
  
    (if (> mode RE) (error "not supported for complex data yet"))

    (setf x_data (compound-data-seq x))
    (setf result_data (compound-data-seq result))
  
    ;; find the mean of y
    (setf val 0.0)
    (setf sum_w 0.0)
    (dotimes (i n)
      (declare (fixnum i))
      (setf dyi (makedouble (aref y i)))
      (when has-w
	(setf dw (makedouble (aref w i)))
	(incf sum_w dw)
	(setf dyi (* dyi dw)))
      (incf val dyi))
    (if (not has-w) (setf sum_w (float n 0.0)))
    (if (<= sum_w 0.0) (error "non positive sum of weights"))
    (setf dy_mean (/ val sum_w))
  
    ;; find the column means
    (dotimes (j p)
      (declare (fixnum j))
      (setf val 0.0)
      (dotimes (i n)
        (declare (fixnum i))
	(setf dxi (makedouble (aref x_data (+ (* p i) j))))
	(when has-w
	  (setf dw (makedouble (aref w i)))
	  (setf dxi (* dxi dw)))
	(incf val dxi))
      (setf (aref x_mean j) (/ val sum_w)))
  
    ;; put 1/sum_w in topleft, means on left, minus means on top
    (setf (aref result_data 0) (/ 1.0 sum_w))
    (dotimes (i p)
      (declare (fixnum i))
      (setf dxi (makedouble (aref x_mean i)))
      (setf (aref result_data (+ i 1)) (- dxi))
      (setf (aref result_data (* (+ i 1) (+ p 2))) dxi))
    (setf (aref result_data (+ p 1)) (- dy_mean))
    (setf (aref result_data (* (+ p 1) (+ p 2))) dy_mean)
  
    ;; put sums of adjusted cross products in body
    (dotimes (i p)
      (declare (fixnum i))
      (dotimes (j p)
        (declare (fixnum j))
	(setf val 0.0)
	(dotimes (k n)
	  (declare (fixnum k))
	  (setf dxik (makedouble (aref x_data (+ (* p k) i))))
	  (setf dxjk (makedouble (aref x_data (+ (* p k) j))))
	  (setf dx_meani (makedouble (aref x_mean i)))
	  (setf dx_meanj (makedouble (aref x_mean j)))
	  (setf dv (* (- dxik dx_meani) (- dxjk dx_meanj)))
	  (when has-w
	    (setf dw (makedouble (aref w k)))
	    (setf dv (* dv dw)))
	  (incf val dv))
	(setf (aref result_data (+ (* (+ i 1) (+ p 2)) (+ j 1))) val)
	(setf (aref result_data (+ (* (+ j 1) (+ p 2)) (+ i 1))) val))
      (setf val 0.0)
      (dotimes (j n)
        (declare (fixnum j))
        (setf dxik (makedouble (aref x_data (+ (* p j) i))))
	(setf dyj (makedouble (aref y j)))
	(setf dx_meani (makedouble (aref x_mean i)))
	(setf dv (* (- dxik dx_meani) (- dyj dy_mean)))
	(when has-w
	  (setf dw (makedouble (aref w j)))
	  (setf dv (* dv dw)))
	(incf val dv))
      (setf (aref result_data (+ (* (+ i 1) (+ p 2)) (+ p 1))) val)
      (setf (aref result_data (+ (* (+ p 1) (+ p 2)) (+ i 1))) val))
    (setf val 0.0)
    (dotimes (j n)
      (declare (fixnum j))
      (setf dyj (makedouble (aref y j)))
      (setf dv (* (- dyj dy_mean) (- dyj dy_mean)))
      (when has-w
        (setf dw (makedouble (aref w j)))
        (setf dv (* dv dw)))
      (incf val dv))
    (setf (aref result_data (+ (* (+ p 1) (+ p 2)) (+ p 1))) val)))

(defun sweep-in-place-front (a rows cols mode k tol)
  (declare-double tol)
  (declare (fixnum rows cols mode k))
  (let ((data nil)
	(pivot 0.0)
	(aij 0.0)
	(aik 0.0)
	(akj 0.0)
	(akk 0.0)
	(RE 1))
    (declare-double pivot aij aik akj akk)
  
    (if (> mode RE) (error "not supported for complex data yet"))
    (if (or (< k 0) (>= k rows) (>= k cols)) (error "index out of range"))
  
    (setf tol (max tol machine-epsilon))
    (setf data (compound-data-seq a))

    (setf pivot (makedouble (aref data (+ (* cols k) k))))
  
    (cond
     ((or (> pivot tol) (< pivot (- tol)))
      (dotimes (i rows)
        (declare (fixnum i))
	(dotimes (j cols)
	  (declare (fixnum j))
	  (when (and (/= i k) (/= j k))
	    (setf aij (makedouble (aref data (+ (* cols i) j))))
	    (setf aik (makedouble (aref data (+ (* cols i) k))))
	    (setf akj (makedouble (aref data (+ (* cols k) j))))
	    (setf aij (- aij (/ (* aik akj) pivot)))
	    (setf (aref data (+ (* cols i) j)) aij))))

      (dotimes (i rows)
        (declare (fixnum i))
	(setf aik (makedouble (aref data (+ (* cols i) k))))
	(when (/= i k)
	  (setf aik (/ aik pivot))
	  (setf (aref data (+ (* cols i) k)) aik)))

      (dotimes (j cols)
        (declare (fixnum j))
	(setf akj (makedouble (aref data (+ (* cols k) j))))
	(when (/= j k)
          (setf akj (- (/ akj pivot)))
	  (setf (aref data (+ (* cols k) j)) akj)))

      (setf akk (/ 1.0 pivot))
      (setf (aref data (+ (* cols k) k)) akk)
      1)
     (t 0))))

(defun make-sweep-matrix (x y &optional w)
"Args: (x y &optional weights)
X is a matrix, Y and WEIGHTS are sequences. Returns the sweep matrix for the
(possibly weighted) regression of Y on X."
  (check-matrix x)
  (check-sequence y)
  (if w (check-sequence w))
  (let ((n (num-rows x))
	(p (num-cols x)))
    (if (/= n (length y)) (error "dimensions do not match"))
    (if (and w (/= n (length w))) (error "dimensions do not match"))
    (let ((mode (max (la-data-mode x) 
                     (la-data-mode x) 
                     (if w (la-data-mode w) 0)))
	  (result (make-array (list (+ p 2) (+ p 2))))
	  (x-mean (make-array p))
	  (y (coerce y 'vector))
	  (w (if w (coerce w 'vector)))
	  (has-w (if w 1 0)))
      (make-sweep-front x y w n p mode has-w x-mean result)
      result)))

(defun sweep-in-place (a k tol)
  (check-matrix a)
  (check-one-fixnum k)
  (check-one-real tol)
  (let ((rows (num-rows a))
	(cols (num-cols a))
	(mode (la-data-mode a)))
    (let ((swept (sweep-in-place-front a rows cols mode k tol)))
      (if (/= 0 swept) t nil))))

(defun sweep-operator (a columns &optional tolerances)
"Args: (a indices &optional tolerances)
A is a matrix, INDICES a sequence of the column indices to be swept. Returns
a list of the swept result and the list of the columns actually swept. (See
MULTREG documentation.) If supplied, TOLERANCES should be a list of real
numbers the same length as INDICES. An index will only be swept if its pivot
element is larger than the corresponding element of TOLERANCES."
  (check-matrix a)
  (check-sequence columns)
  (if tolerances (check-sequence tolerances))
  (check-real a)
  (check-fixnum columns)
  (if tolerances (check-real tolerances))
  (do ((tol .0000001)
       (result (copy-array a))
       (swept-columns nil)
       (columns (coerce columns 'list) (cdr columns))
       (tolerances (if (consp tolerances) (coerce tolerances 'list))
		   (if (consp tolerances) (cdr tolerances))))
      ((null columns) (list result swept-columns))
      (let ((col (first columns))
	    (tol (if (consp tolerances) (first tolerances) tol)))
	(if (sweep-in-place result col tol)
	    (setf swept-columns (cons col swept-columns))))))


;;;;
;;;; AX+Y
;;;;

;;;*** this could probably be made more efficient ***
(defun ax+y (a x y &optional lower)
"Args (a x y &optional lower)
Returns (+ (matmult A X) Y). If LOWER is not nil, A is taken to be lower
triangular."
  (check-square-matrix a)
  (check-sequence x)
  (check-sequence y)
  (check-real a)
  (check-real x)
  (check-real y)
  (let* ((n (num-rows a))
	 (result (make-list n))
	 (a (compound-data-seq a)))
    (declare (fixnum n))
    (if (or (/= n (length x)) (/= n (length y)))
	(error "dimensions do not match"))
    (do* ((tx (make-next-element x) (make-next-element x))
	  (ty (make-next-element y))
	  (tr (make-next-element result))
	  (i 0 (+ i 1))
	  (start 0 (+ start n))
	  (end (if lower (+ i 1) n) (if lower (+ i 1) n)))
	 ((<= n i) result)
       (declare (fixnum i start end))
       (let ((val (get-next-element ty i)))
	 (dotimes (j end)
           (declare (fixnum j))
	   (setf val (+ val (* (get-next-element tx j)
			       (aref a (+ start j))))))
	 (set-next-element tr i val)))))

;;;;
;;;; Maximization and Numerical Derivatives
;;;;

(defvar *maximize-callback-function* nil)
(defvar *maximize-callback-arg* nil)

(defun data2double (n data ptr)
  (declare (fixnum n))
  (let* ((seq (compound-data-seq data))
	 (elem (make-next-element seq)))
    (if (/= (length seq) n) (error "bad data size"))
    (dotimes (i n) 
      (declare (fixnum i))
      (la-put-double ptr i (get-next-element elem i)))))

(defun maximize-callback (n px pfval pgrad phess pderivs)
  (la-vector-to-data px n mode-re *maximize-callback-arg*)
  (let* ((val (funcall *maximize-callback-function* *maximize-callback-arg*))
	 (derivs (if (consp val) (- (length val) 1) 0)))
    (la-put-integer pderivs 0 derivs)
    (la-put-double pfval 0 (if (consp val) (first val) val))
    (if (<= 1 derivs) (data2double n (second val) pgrad))
    (if (<= 2 derivs) (data2double (* n n) (third val) phess))))

(defun numgrad (f x &optional scale (h -1.0))
"Args: (f x &optional scale derivstep)
Computes the numerical gradient of F at X."
  (check-sequence x)
  (check-real x)
  (when scale
	(check-sequence scale)
	(check-real scale))
  (check-one-real h)
  (let* ((n (length x))
	 (result (make-list n)))
    (if (and scale (/= n (length scale)))
	(error "scale not the same length as x"))
    (let ((*maximize-callback-function* f)
	  (*maximize-callback-arg* (make-list n)))
      (let ((px (la-data-to-vector x mode-re))
	    (pgrad (la-vector n mode-re))
	    (pscale (la-data-to-vector
		     (if scale scale (make-list n :initial-element 1.0))
		     mode-re)))
	(unwind-protect
	    (progn
	      (numgrad-front n px pgrad h pscale)
	      (la-vector-to-data pgrad n mode-re result))
	  (la-free-vector px)
	  (la-free-vector pgrad)
	  (la-free-vector pscale))))
      result))

(defun numhess (f x &optional scale (h -1.0) all)
"Args: (f x &optional scale derivstep)
Computes the numerical Hessian matrix of F at X."
  (check-sequence x)
  (check-real x)
  (when scale
	(check-sequence scale)
	(check-real scale))
  (check-one-real h)
  (let* ((n (length x))
	 (result (if all 
		     (list nil (make-list n) (make-array (list n n)))
		     (make-array (list n n)))))
    (if (and scale (/= n (length scale)))
	(error "scale not the same length as x"))
    (let ((*maximize-callback-function* f)
	  (*maximize-callback-arg* (make-list n)))
      (let ((hess-data (compound-data-seq (if all (third result) result)))
	    (px (la-data-to-vector x mode-re))
	    (pf (la-vector 1 mode-re))
	    (pgrad (la-vector n mode-re))
	    (phess (la-vector (* n n) mode-re))
	    (pscale (la-data-to-vector
		     (if scale scale (make-list n :initial-element 1.0))
		     mode-re)))
	(unwind-protect
	    (progn
	      (numhess-front n px pf pgrad phess h pscale)
	      (when all
		    (setf (first result) (la-get-double pf 0))
		    (la-vector-to-data pgrad n mode-re (second result)))
	      (la-vector-to-data phess (* n n) mode-re hess-data))
	  (la-free-vector pf)
	  (la-free-vector px)
	  (la-free-vector pgrad)
	  (la-free-vector phess)
	  (la-free-vector pscale))))
    result))

(defun init-minfo-ipar-values (n ipars)
  (let* ((TRUE 1)
	 (FALSE 0)
	 (k 0)
	 (m 0)
	 (itnlimit -1)
	 (backtrack TRUE)
	 (verbose 0)
	 (vals_suppl FALSE)
	 (exptilt TRUE)
	 (count 0)
	 (termcode 0))
    (setf (aref ipars 0) n)
    (setf (aref ipars 1) m)
    (setf (aref ipars 2) k)
    (setf (aref ipars 3) itnlimit)
    (setf (aref ipars 4) backtrack)
    (setf (aref ipars 5) verbose)
    (setf (aref ipars 6) vals_suppl)
    (setf (aref ipars 7) exptilt)
    (setf (aref ipars 8) count)
    (setf (aref ipars 9) termcode)))

(defun init-minfo-dpar-values (h dpars)
  (let ((typf 1.0)
	(gradtol -1.0)
	(steptol -1.0)
	(maxstep -1.0)
	(dflt 0.0)
	(tilt 0.0)
	(newtilt 0.0)
	(hessadd 0.0))
    (setf (aref dpars 0) typf)
    (setf (aref dpars 1) h)
    (setf (aref dpars 2) gradtol)
    (setf (aref dpars 3) steptol)
    (setf (aref dpars 4) maxstep)
    (setf (aref dpars 5) dflt)
    (setf (aref dpars 6) tilt)
    (setf (aref dpars 7) newtilt)
    (setf (aref dpars 8) hessadd)))

(defun init-minfo-internals (n h internals)
  (let ((ipars (aref internals 8))
	(dpars (aref internals 9)))
    (init-minfo-ipar-values n ipars)
    (init-minfo-dpar-values h dpars)))

(defun new-minfo-internals (f x &key scale ((:derivstep h) -1.0))
  (check-sequence x)
  (check-real x)
  (check-one-real h)
  (let ((n (length x)))
    (when scale
	  (check-sequence scale)
	  (check-real scale)
	  (if (/= n (length scale)) (error "scale and x not the same length")))
    (let ((internals (make-array 12)))
      (setf (aref internals 0) f)
      (setf (aref internals 3) (if (consp x) (copy-list x) (coerce x 'list)))
      (setf (aref internals 4)
	    (if scale (copy-seq scale) (make-array n :initial-element 1.0)))
      (setf (aref internals 5) (make-list (+ 1 n (* n n))))
      (setf (aref internals 8) (make-array 10))
      (setf (aref internals 9) (make-array 9))
      (init-minfo-internals n h internals)
      internals)))

(defun minfo-maximize (internals &optional verbose)
  (let* ((f (aref internals 0))
	 (x (aref internals 3))
	 (fvals (aref internals 5))
	 (n (length x))
	 (v (if verbose (if (integerp verbose) verbose 1) -1)))
    (setf (aref internals 3) (copy-list x))
    (setf (aref internals 5) (copy-list fvals))
    (let ((*maximize-callback-function* f)
	  (*maximize-callback-arg* (make-list n)))
      (let* ((x (aref internals 3))
	     (scale (aref internals 4))
	     (fvals (aref internals 5))
	     (ip (aref internals 8))
	     (dp (aref internals 9))
	     (px (la-data-to-vector x mode-re))
	     (pscale (la-data-to-vector scale mode-re))
	     (pfvals (la-vector (length fvals) mode-re))
	     (pip (la-data-to-vector ip mode-in))
	     (pdp (la-data-to-vector dp mode-re)))
	(unwind-protect
	    (progn
	      (base-minfo-maximize px pfvals pscale pip pdp v))
	  (la-vector-to-data px n mode-re x)
	  (la-vector-to-data pfvals (+ 1 n (* n n)) mode-re fvals)
	  (la-vector-to-data pip (length ip) mode-in ip)
	  (la-vector-to-data pdp (length dp) mode-re dp))
	(get-buf)))))

;;;;
;;;; Miscellaneous Routines
;;;;


(defun split-list (x n)
"Args: (list cols)
Returns a list of COLS lists of equal length of the elements of LIST.
Example: (split-list '(1 2 3 4 5 6) 2) returns ((1 2 3) (4 5 6))"
  (check-one-fixnum n)
  (if (/= (rem (length x) n) 0) (error "length not divisible by ~a" n))
  (flet ((next-split ()
           (let ((result nil)
                 (end nil))
             (dotimes (i n result)
               (declare (fixnum i))
               (let ((c-elem (list (first x))))
                 (cond ((null result)
                        (setf result c-elem)
                        (setf end result))
                       (t 
                        (setf (rest end) c-elem)
                        (setf end (rest end)))))
               (setf x (rest x))))))
    (let ((result nil)
          (end nil)
          (k (/ (length x) n)))
      (declare (fixnum k))
      (dotimes (i k result)
        (declare (fixnum i))
        (let ((c-sub (list (next-split))))
          (cond ((null result)
                 (setf result c-sub)
                 (setf end result))
                (t 
                 (setf (rest end) c-sub)
                 (setf end (rest end)))))))))
          
