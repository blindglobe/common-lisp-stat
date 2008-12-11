;;; -*- mode: lisp -*-
;;; Copyright (c) 2005--2008, by A.J. Rossini <blindglobe@gmail.com>
;;; See COPYRIGHT file for any additional restrictions (BSD license).
;;; Since 1991, ANSI was finally finished.  Edited for ANSI Common Lisp.
;;;

(in-package #:lisp-matrix-stat-linalg)

;;;;
;;;; Spline Interpolation
;;;;

(cffi:defcfun ("ccl_range_to_rseq" ccl-range-to-rseq)
    :int (x size-t) (y :pointer) (z size-t) (u :pointer))
(defun la-range-to-rseq (x y z u)
  (ccl-range-to-rseq x y z u))

(cffi:defcfun ("ccl_spline_front" ccl-spline-front)
    :int (x size-t) (y :pointer) (z :pointer) (u size-t) (v :pointer) (w :pointer) (a :pointer))
(defun spline-front (x y z u v w a) 
  (ccl-spline-front x y z u v w a))

;;;;
;;;; Kernel Density Estimators and Smoothers
;;;;

(cffi:defcfun ("ccl_kernel_dens_front" ccl-kernel-dens-front)
    :int (x :pointer) (y size-t) (z :double) (u :pointer) (v :pointer) (w size-t) (a :int))
(defun kernel-dens-front (x y z u v w a)
  (ccl-kernel-dens-front x y (float z 1d0) u v w a))

(cffi:defcfun ("ccl_kernel_smooth_front" ccl-kernel-smooth-front)
    :int (x :pointer) (y :pointer) (z size-t) (u :double) (v :pointer) (w :pointer) (a size-t) (b :int))
(defun kernel-smooth-front (x y z u v w a b)
  (ccl-kernel-smooth-front x y z (float u 1d0) v w a b))

;;;;
;;;; Lowess Smoother Interface
;;;;

(cffi:defcfun ("ccl_base_lowess_front" ccl-base-lowess-front)
  :int (x :pointer) (y :pointer) (z size-t) (u :double) (v size-t) (w :double) (a :pointer) (b :pointer) (c :pointer))
(defun base-lowess-front (x y z u v w a b c)
  (ccl-base-lowess-front x y z (float u 1d0) v (float w 1d0) a b c))

;;;;
;;;; FFT
;;;;

(cffi:defcfun ("ccl_fft_front" ccl-fft-front)
    :int (x size-t) (y :pointer) (z :pointer) (u :int))
(defun fft-front (x y z u) 
  (ccl-fft-front x y z u))



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
       (let* ((px (la-data-to-vector ,x +mode-re+))
	      (py (if ,is-reg (la-data-to-vector ,y +mode-re+)))
	      (pxs (if supplied 
		       (la-data-to-vector ,xvals +mode-re+)
		       (la-vector ns +mode-re+)))
	      (pys (la-vector ns +mode-re+)))
	 (unless supplied (la-range-to-rseq n px ns pxs))
	 (unwind-protect
	     (progn ,@body
		    (la-vector-to-data pxs ns +mode-re+ (first result))
		    (la-vector-to-data pys ns +mode-re+ (second result)))
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
    (let ((work (la-vector (* 2 n) +mode-re+))
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
  (with-smoother-data (x nil xvals nil) ;; warning about deleting unreachable code is TRUE -- 2nd arg=nil!
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
      ;; if we get the Lisp version ported from C, uncomment below and
      ;; comment above.  (thanks to Carlos Ungil for the initial CFFI
      ;; work).
      ;;(kernel-smooth-Cport px py n width pxs pys ns code)
      (if (/= 0 error) (error "bad kernel density data")))))


#|
(defun kernel-smooth-Cport (px py n width ;;wts wds ;; see above for mismatch?
			    xs ys ns ktype)
  "Port of kernel_smooth (Lib/kernel.c) to Lisp.
FIXME:kernel-smooth-Cport :  This is broken.
Until this is fixed, we are using Luke's C code and CFFI as glue."
  (declare (ignore width xs))
  (cond ((< n 1) 1.0)
	((and (< n 2) (<= width 0)) 1.0)
	(t (let* ((xmin (min px))
		  (xmax (max px))
		  (width (/ (- xmax xmin) (+ 1.0 (log n)))))
	     (dotimes  (i (- ns 1))
	       (setf (aref ys i)
		     (let ((wsum 0.0)
			   (ysum 0.0))
		       (dotimes (j (- n 1))   )
;;;possible nasty errors...
;;		       (let* 
;;			     ((lwidth (if wds (* width (aref wds j)) width))
;;			      (lwt (* (kernel-Cport (aref xs i) (aref px j) lwidth ktype) ;; px?
;;				      (if wts (aref wts j) 1.0))))
;;			   (setf wsum (+ wsum lwt))
;;			   (setf ysum (if py (+ ysum (* lwt (aref py j)))))) ;; py? y?
;;
;;; end of errors
		       (if py
			   (if (> wsum 0.0) 
			       (/ ysum wsum)
			       0.0)
			   (/ wsum n)))))
	     (values ys)))))
|#


(defun kernel-Cport (x y w ktype)
  "Port of kernel() (Lib/kernel.c) to Lisp.
x,y,w are doubles, type is an integer"
  (if (<= w 0.0)
      0.0
      (let ((z (- x y)))
	(cond ((eq ktype "B") 
	       (let* ((w (* w 2.0))
		      (z (* z 0.5)))
		 (if (and (> z -0.5)
			  (< z 0.5))
		     (/ (/ (* 15.0 (* (- 1.0 (* 4 z z))  ;; k/w
				      (- 1.0 (* 4 z z)))) ;; k/w
			   8.0)
			w)
		     0)))
	      ((eq ktype "G")
	       (let* ((w (* w 0.25))
		      (z (* z 4.0))
		      (k (/ (exp (* -0.5 z z))
			    (sqrt (* 2 PI)))))
		 (/ k w)))
	      ((eq ktype "U")
	       (let* ((w (* 1.5 w))
		      (z (* z 0.75))
		      (k (if (< (abs z) 0.5)
			     1.0
			     0.0)))
		 (/ k w)))
	      ((eq ktype "T")
	       (cond ((and (> z -1.0)
			   (< z 0.0))
		      (+ 1.0 z))  ;; k
		     ((and (> z 0.0)
			   (< z 1.0))
		      (- 1.0 z))  ;; k
		     (t 0.0)))
	      (t (values 0.0))))))
		

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
    (let ((x (la-data-to-vector s1 +mode-re+))
	  (y (la-data-to-vector s2 +mode-re+))
	  (ys (la-vector n +mode-re+))
	  (rw (la-vector n +mode-re+))
	  (res (la-vector n +mode-re+))
	  (error 0))
      (unwind-protect
	  (progn
	    (setf error (base-lowess-front x y n f nsteps delta ys rw res))
	    (la-vector-to-data ys n +mode-re+ result))
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

;;;
;;; FFT
;;;
;;; FIXME:ajr
;;; ??replace with matlisp:fft  and matlisp:ifft (the latter for inverse mapping)
;;;
(defun fft (x &optional inverse)
"Args: (x &optional inverse)
Returns unnormalized Fourier transform of X, or inverse transform if INVERSE
is true."
  (check-sequence x)
  (let* ((n (length x))
	 ;;(mode (la-data-mode x))
	 (isign (if inverse -1 1))
	 (result (if (consp x) (make-list n) (make-array n))))
    (let ((px (la-data-to-vector x +mode-cx+))
	  (work (la-vector (+ (* 4 n) 15) +mode-re+)))
      (unwind-protect
	  (progn
	    (fft-front n px work isign)
	    (la-vector-to-data px n +mode-cx+ result))
	(la-free-vector px)
	(la-free-vector work))
      result)))

;;;
;;; SWEEP Operator: FIXME: use matlisp
;;;

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
    (declare (long-float val dxi dyi dv dw sum_w dxik dxjk dyj
		    dx_meani dx_meanj dy_mean)) ;; originally "declare-double" macro
  
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

;;; FIXME: use matlisp
(defun sweep-in-place-front (a rows cols mode k tol)
  "Sweep algorithm for linear regression."
  (declare (long-float tol))
  (declare (fixnum rows cols mode k))
  (let ((data nil)
	(pivot 0.0)
	(aij 0.0)
	(aik 0.0)
	(akj 0.0)
	(akk 0.0)
	(RE 1))
    (declare (long-float pivot aij aik akj akk))
  
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

;; FIXME: use matlisp
(defun make-sweep-matrix (x y &optional w)
  "Args: (x y &optional weights)
 X is matrix-like, Y and WEIGHTS are vector-like. Returns the sweep matrix of the
 (weighted) regression of Y on X"
   (assert (typep x 'matrix-like))
   (assert (typep y 'vector-like))
   (if w (assert (typep w 'vector-like)))
   (let ((n (matrix-dimension x 0))
	 (p (matrix-dimension x 1)))
     (if (/= n (length y)) (error "dimensions do not match"))
     (if (and w (/= n (length w))) (error "dimensions do not match"))
     (let ((mode (max (la-data-mode x) 
                     (la-data-mode x) 
                     (if w (la-data-mode w) 0)))
	   (result (make-matrix (+ p 2) (+ p 2))))
       (x-mean (make-vector p))
       (has-w (if w 1 0))
       (make-sweep-front x y w n p mode has-w x-mean result)
       result)))

 (defun sweep-in-place (a k tol)
  (assert (typep a 'matrix-like))
  (check-one-fixnum k)
  (check-one-real tol)
  (let ((rows (num-rows a))
	(cols (num-cols a))
	(mode (la-data-mode a)))
    (let ((swept (sweep-in-place-front
		  a
		  (matrix-dimensions a 0)
		  (matrix-dimensions a 1)
		  mode k tol)))
      (if (/= 0 swept) t nil))))

(defun sweep-operator (a columns &optional tolerances)
  "Args: (a indices &optional tolerances)

A is a matrix, INDICES a sequence of the column indices to be
swept. Returns a list of the swept result and the list of the columns
actually swept. (See MULTREG documentation.) If supplied, TOLERANCES
should be a list of real numbers the same length as INDICES. An index
will only be swept if its pivot element is larger than the
corresponding element of TOLERANCES."

  (check-matrix a)
  (if (not (typep columns 'sequence))
      (setf columns (list columns)))
  (check-sequence columns)
  (if tolerances
      (progn
	(if (not (typep tolerances 'sequence))
	    (setf tolerances (list tolerances)))
	(check-sequence tolerances)))

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


;;;
;;; AX+Y
;;;

;;; matlisp:axpy
;;;
(defun ax+y (a x y &optional lower)
"Args (a x y &optional lower)
Returns (+ (matmult A X) Y). If LOWER is not nil, A is taken to be lower
triangular.
This could probably be made more efficient."
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
;;;; Linear Algebra Functions
;;;;

(defun matrix (dim data)
"Args: (dim data)
returns a matrix of dimensions DIM initialized using sequence DATA
in row major order." 
  (let ((dim (coerce dim 'list))
        (data (coerce data 'list)))
    (make-array dim :initial-contents (split-list data (nth 1 dim)))))

(defun flatsize (x)
  (length x))  ;; FIXME: defined badly!!

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
    (list x y delta f steps)));; (|base-lowess| x y f steps delta))))
