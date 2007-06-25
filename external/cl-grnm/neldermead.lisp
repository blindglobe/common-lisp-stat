(in-package :neldermead)

(defclass cached-simplex-data ()
  ((pseudopivot	:initform nil)
   (q-factor :initform nil)
   (side-vectors :initform nil)
   (r-factor :initform nil)
   (dv :initform nil
       :initarg :dv
       :accessor dv)
   (dmin :initform nil
	 :initarg :dmin
	 :accessor dmin)
   (best-before-reshape :initform nil
			:initarg :best-before-reshape
			:accessor best-before-reshape)))

(defclass nm-simplex ()
  ((x :accessor x
      :initarg :x)
   (fx :accessor fx
       :initarg :fx)
   (pmap :accessor pmap
	 :initarg :pmap)
   (data :accessor data
	 :initform nil)))

(defclass grid ()
  ((z :accessor grid-z :initarg :z)
   (delta :accessor delta :initarg :delta)))

(defvar *verbose-level* 1)

(defmethod print-object ((s nm-simplex) stream)
  (format stream "#<NM-SIMPLEX")
  (case *verbose-level*
    (2 (let* ((fx (fx s))
	      (pmap (pmap s))
	      (fmt "~12,3,3E")
	      (n (length pmap)))

	 (dotimes (i n)
	   (terpri stream)
	   (if fx (format stream fmt (fk s i))
	       (format stream " -- "))

	   (format stream " :")
	   (dotimes (j (- n 1))
	     (format stream fmt (aref (xk s i) j))))))
    (1 (format stream " D=~A Best=~A " (dimension s)
	       (if (fx s)
		   (format nil "~16,7,3E" (fk s 0))
		   "--"))))
  (format stream ">"))

(defmethod xk ((s nm-simplex) k)
  (aref (x s) (aref (pmap s) k)))

(defmethod (setf xk) (nv (s nm-simplex) k)
  (setf (aref (x s) (aref (pmap s) k)) nv))

(defmethod fk ((s nm-simplex) k)
  (aref (fx s) (aref (pmap s) k)))

(defmethod (setf fk) (nv (s nm-simplex) k)
  (setf (aref (fx s) (aref (pmap s) k)) nv))

(defmethod sort-simplex ((s nm-simplex))
  (sort (pmap s) #'< :key #'(lambda (k) (aref (fx s) k)))
  s)

(defmethod dimension ((s nm-simplex))
  (- (length (pmap s)) 1))

;; simple additive simplex generator
(defun initial-simplex (x0
			&key (displace 0.1d0))
  (let* ((n (length x0))
	 (x (make-array (+ n 1)))
	 (pmap (make-array (+ n 1) :element-type 'fixnum)))

    (setf (aref x 0) x0
	  (aref pmap 0) 0)

    (dotimes (k n)
      (let ((xk (copy-seq x0)))

	(incf (aref xk k)
	      (if (numberp displace) displace
		  (aref displace k)))
	(setf (aref x (+ k 1)) xk
	      (aref pmap (+ k 1)) (+ k 1))))

    (make-instance 'nm-simplex
		   :x x :fx nil :pmap pmap)))

;; The simplex generator from the article, more or less.
(defun default-initial-simplex (x0)
  (initial-simplex x0
		   :displace
		   (map 'vector
			#'(lambda (v)
			    (max 0.00025d0
				 (abs (* v 0.05d0))))
			x0)))

(defmethod maybe-fill-simplex ((s nm-simplex) f)
  (if (fx s) s
      (let* ((n (length (x s)))
	     (fx (make-array n :element-type 'double-float)))
	(loop :for i :from 0 :below n :do
	  (setf (aref fx i) (funcall f (xk s i))))

	(setf (fx s) fx)
	s)))

;; substitutes the worst point with x/fx. Assumes simplex is sorted.
(defmethod improve ((s nm-simplex) x fx)
  (let ((last (length x)))
    (setf (xk s last) x
	  (fk s last) fx

	  (data s) nil)
	  
    (sort-simplex s)))

(defmethod cached-slot ((s nm-simplex) slot computer)
  (if (and (data s) (slot-value (data s) slot))
      (slot-value (data s) slot)
      (setf (data s) (if (data s) (data s)
			 (make-instance 'cached-simplex-data))
	    (slot-value (data s) slot) (funcall computer))))

(defmethod pseudopivot ((s nm-simplex))
  (cached-slot
   s 'pseudopivot
	       
   #'(lambda ()
       (let* ((n (- (length (pmap s)) 1))
	      (xbar (make-array n
				:element-type 'double-float
				:initial-element 0.0d0)))
	 
	 (dotimes (i n)
	   (setf xbar (v+w*c xbar (xk s (+ i 1)) (/ 1.0d0 n))))

	 xbar))))

(defmethod side-vectors ((s nm-simplex))
  (cached-slot
   s 'side-vectors

   #'(lambda ()
       (let* ((n (- (length (pmap s)) 1))
	      (sv (make-array n)))

	 (dotimes (i n)
	   (setf (aref sv i) (v+w*c (xk s (+ i 1)) (xk s 0) -1)))

	 sv))))

(defun simplex-qr-thing (sidev)
  (let* ((n (length sidev))
	 (norms (make-array n :element-type 'double-float))
	 (pmap (make-array n :element-type 'fixnum))
	 (mat (make-array (list n n) :element-type 'double-float)))

    (dotimes (i n)
      (setf (aref pmap i) i
	    (aref norms i) (norm (aref sidev i))))

    (sort pmap #'> :key #'(lambda (k) (aref norms k)))

    (dotimes (i n)
      (dotimes (j n)
	(setf (aref mat i j)
	      (aref (aref sidev (aref pmap j)) i))))

    (multiple-value-bind (r q) (qr-factorization mat)
      (values q r pmap))))

(defun qrthing-closure (s n)
  #'(lambda ()
      (multiple-value-bind (q r p)
	  (simplex-qr-thing (side-vectors s))
	(setf (slot-value (data s) 'q-factor) q
	      (slot-value (data s) 'r-factor) r)

	(elt (list q r p) n))))

(defmethod q-factor ((s nm-simplex))
  (cached-slot s 'q-factor

	       (qrthing-closure s 0)))

(defmethod r-factor ((s nm-simplex))
  (cached-slot s 'r-factor
	       (qrthing-closure s 1)))

;; A Nelder-Mead iteration.
(defun nm-iteration
    (simplex f &key
     verbose
     (gamma_reflect 1.0d0)
     (gamma_expand 2.0d0)
     (gamma_outer_contraction 0.5d0)
     (gamma_inner_contraction -0.5d0)
     (gamma_shrink 0.5d0))


  (let* ((n (- (length (x simplex)) 1))

	 (x_cb (make-array n
			   :element-type 'double-float
			   :initial-element 0.0d0))
	 (x_cb-x_n (make-array n
			       :element-type 'double-float
			       :initial-element 0.0d0)))

    (labels ((newpoint (gamma)
	       (let ((np (v+w*c x_cb x_cb-x_n gamma)))
		 (values np (funcall f np))))
	       
	     (accept (xx ff)
	       (improve simplex xx ff))

	     (shrink ()
	       (let ((x0 (xk simplex 0)))
		 (loop for i from 1 to n do
		       (let* ((newx (v+w*c (v*c x0
						(- 1.0d0 gamma_shrink))
					   (xk simplex i) gamma_shrink))
			      (newf (funcall f newx)))

			 (setf (xk simplex i) newx
			       (fk simplex i) newf)

			 (sort-simplex simplex))))))

      ;; compute centroid
      (dotimes (i n)
	(setf x_cb (v+w*c x_cb (xk simplex i) (/ 1.0d0 n))))

      (setf x_cb-x_n (v+w*c x_cb (xk simplex n) -1.0d0))

      ;; 2. Reflect
      (multiple-value-bind (xr fr)
	  (newpoint gamma_reflect)
	(if (and (<= (fk simplex 0) fr) (< fr (fk simplex (- n 1))))
	    (accept xr fr)
	    ;; 3. expand
	    (if (< fr (fk simplex 0))
		(multiple-value-bind (xe fe)
		    (newpoint gamma_expand)
		  (if (< fe fr)
		      (accept xe fe)
		      (accept xr fr)))
		;; 4. contract or shrink
		(if (<= (fk simplex (- n 1)) fr)
		    (if (< fr (fk simplex n))
			;;outer contraction
			(multiple-value-bind (xc fc)
			    (newpoint gamma_outer_contraction)
			  (if (or (= n 2)
				  (<= fc fr)) ;; 1D shrink is
			      ;; equivalent to
			      ;; acceptance of this
			      ;; point
			      (accept xc fc)
			      (shrink)))
			;; inner contraction
			(multiple-value-bind (xcc fcc)
			    (newpoint gamma_inner_contraction)
			  (if (< fcc (fk simplex n)) (accept xcc fcc)
			      (shrink))))

		    (shrink)))))
      
      (when verbose (format t "~S~%" simplex))
      simplex)))

;; The test returns true if the volume of the paralelepiped spanned by
;; the vertices of the simplex is smaller than that of an n-cube of
;; side cside.
(defun pp-volume-test (cside)
  #'(lambda (simplex)
      (let* ((mat (r-factor simplex)))

	(let ((det 1.0d0))
	  (dotimes (i (dimension simplex))
	    (setf det (* det (aref mat i i))))

	  (< (abs det) (expt cside (dimension simplex)))))))

(defun nm-optimize (objective-function initial-guess &key
		    (max-function-calls 100000)
		    (convergence-p (burmen-et-al-convergence-test))
		    verbose)

  (let ((simplex (if (typep initial-guess 'nm-simplex)
		     initial-guess
		     (default-initial-simplex initial-guess)))
	(fvcount 0))
    (labels ((rigged-f (v)
	       (incf fvcount)
	       (funcall objective-function v))
	     (converged-p (s)
	       (or (funcall convergence-p s)
		   (> fvcount max-function-calls))))

      (when verbose
	(format t "Initial simplex: ~%~A~%---~%" simplex))

      (maybe-fill-simplex simplex #'rigged-f)

      (loop :until (converged-p simplex)
	    :do
	    (nm-iteration simplex #'rigged-f :verbose verbose))

      (values (xk simplex 0) (fk simplex 0) simplex fvcount))))

(defmethod restrict ((grid grid) point)
  (let ((new (copy-seq point))
	(n (length point))
	(delta (delta grid))
	(z (grid-z grid)))

    (dotimes (i n)
      (setf (aref new i)
	    (+ (* (aref delta i)
		  (floor
		   (+ (/ (- (aref new i)
			    (aref z i))
			 (aref delta i))
		      0.5d0)))
	       (aref z i))))

    new))


;; Some parameters. Look at Burmen et al for further details.
(defparameter *psi* 1.0d-6)
(defparameter *biglambda* (/ 0.5d0 double-float-epsilon))
(defparameter *tau-r* (* 2.0d0 double-float-epsilon))
(defparameter *tau-a* (expt least-positive-double-float (/ 1.0d0 3.0d0)))
(defparameter *smalllambda* 2)

(defvar *breakdown*)

(defmethod maybe-reshape ((s nm-simplex) (g grid) ff &key force)
  (let ((n (length (side-vectors s)))
	(biglambda *biglambda*)
	(smalllambda *smalllambda*)
	(reshaped-p nil))
    
    (labels ((degenerate-p ()
	       (let* ((r (r-factor s))
		      (ax (loop for i from 0 below n
				minimizing (abs (aref r i i)))))
		       
		 (< ax
		    (/ (* *psi*
			  (norm (delta g))
			  (sqrt (float n 1.0d0)))
		       2)))))

      (when (or force (degenerate-p))

	(setf reshaped-p t)
	
	(let ((r (r-factor s))
	      (q (q-factor s))
	      (|det| 1.0d0)
	      (dv (make-array (+ n 1)))
	      (dmin-norm nil)
	      (dmin nil)
	      (|Delta| (norm (delta g))))

	  (setf (aref dv 0) (xk s 0))

	  (dotimes (i n)
	    (let* ((di (make-array n :element-type 'double-float))
		   (rii (aref r i i))
		   (sgn[rii] (if (>= rii 0) 1 -1))
		   (|rii| (abs rii))
		   (quot (* (sqrt (float n 0.0d0))
			    |Delta|
			    0.5d0))
		   (minc (min |rii|
			      (* biglambda quot)))
		   (maxc (max (* smalllambda quot) minc))
		   (dfkt (* sgn[rii] maxc)))

	      (setf |det| (* |det| |rii|))

	      (dotimes (j n)
		(setf (aref di j)
		      (* dfkt (aref q j i))))
	      
	      (when (or (not dmin-norm) (< (abs dfkt) dmin-norm))
		(setf dmin-norm (abs dfkt)
		      dmin di))

	      (setf (aref dv (+ i 1)) di)

	      (let* ((nxi (restrict g (v+w*c (xk s 0) di 1.0d0)))
		     (fxi (funcall ff nxi)))

		(setf (xk s (+ i 1)) nxi
		      (fk s (+ i 1)) fxi))))

	  ;; Looks like a very extreme situation, but can actually
	  ;; happen.
	  (when (= |det| 0.0d0)
	    (setf *breakdown* t))
	  
	  ;; Most cached data is invalid, but better keep the reshape
	  ;; data, which might be needed for shrinking the simplex.
	  (setf (data s)
		(make-instance 'cached-simplex-data
			       :dv dv
			       :dmin dmin
			       :best-before-reshape (fk s 0)))
	  
	  (sort-simplex s))))
    
    reshaped-p))

;;; Nelder Mead iteration variant from Burmen et al.  Does not shrink,
;;; "failing" instead. Acceptance criteria for the contraction points
;;; are stricter.
;;;
;;; Iterates are restricted to a grid.
(defun nm-iteration-burmen-et-al
    (simplex f grid &key
     verbose
     (gamma_reflect 1.0d0)
     (gamma_expand 2.0d0)
     (gamma_outer_contraction 0.5d0)
     (gamma_inner_contraction -0.5d0))
     

  (let ((n (- (length (x simplex)) 1))
	(failure t))

    (let ((x_cb (make-array n
			    :element-type 'double-float
			    :initial-element 0.0d0))
	  (x_cb-x_n (make-array n
				:element-type 'double-float
				:initial-element 0.0d0)))

      (labels ((newpoint (gamma)
		 (let ((np (restrict grid
				     (v+w*c x_cb x_cb-x_n gamma))))
		   (values np (funcall f np))))
	       
	       (accept (xx ff)
		 (improve simplex xx ff)
		 (setf failure nil)))

	;; compute centroid
	(dotimes (i n)
	  (setf x_cb (v+w*c x_cb (xk simplex i) (/ 1.0d0 n))))

	(setf x_cb-x_n (v+w*c x_cb (xk simplex n) -1.0d0))

	;; 2. Reflect
	(multiple-value-bind (xr fr)
	    (newpoint gamma_reflect)
	  (if (and (<= (fk simplex 0) fr) (< fr (fk simplex (- n 1))))
	      (accept xr fr)
	      ;; 3. expand
	      (if (< fr (fk simplex 0))
		  (multiple-value-bind (xe fe)
		      (newpoint gamma_expand)
		    (if (< fe fr)
			(accept xe fe)
			(accept xr fr)))
		  ;; 4. contract or fail ABurmen & al have swapped
		  ;; inner and outers - maybe a typo. (?) No, just a
		  ;; different variant.
		  (if (<= (fk simplex (- n 1)) fr)
		      (if (< fr (fk simplex n))
			  ;;outer contraction
			  (multiple-value-bind (xc fc)
			      (newpoint gamma_outer_contraction)
			    (if (<= fc (fk simplex (- n 1)))
				(accept xc fc)
				))
			  ;; inner contraction
			  (multiple-value-bind (xcc fcc)
			      (newpoint gamma_inner_contraction)
			    (if (< fcc (fk simplex (- n 1)))
				(accept xcc fcc))))))))))

    (sort-simplex simplex)
    (when verbose (format t "~S~%" simplex))
    (values simplex failure)))

;; Convergence test used in the article
(defun burmen-et-al-convergence-test (&key
				      (tol-x 1.0d-8)
				      (tol-f 1.0d-15)
				      (rel 1.0d-15))

  #'(lambda (s)
      (let* ((sv (side-vectors s))
	     (fdiff (abs (- (fk s 0) (fk s (dimension s)))))
	     (vijmax (loop for v across sv maximizing
			   (loop for x across v maximizing
				 (abs x))))
	     (xbest (xk s 0))
	     (|xi|max (loop for z across xbest maximizing (abs z))))

	(and (< fdiff (max tol-f (* rel (fk s 0))))
	     (< vijmax (max tol-x (* rel |xi|max)))))))
      
      

(defun regrid (g x1 dmin fkt)
  (let* ((dmin (v*c dmin fkt))
	 (n (length dmin))
	 (|dmin| (norm dmin))
	 (delta (delta g))
	 (newd (copy-seq delta)))
    
    (setf (grid-z g) x1)
    
    (dotimes (i (length dmin))
      (setf (aref newd i)
	    
	    (max (min (max (/ (abs (aref dmin i))
			      (* *smalllambda* 250 n))

			   (/ |dmin|
			      (* *smalllambda* 250 (expt n (/ 3.0d0 2.0d0)))))
		      (aref delta i))
		 (* *tau-r* (aref x1 i))

		 *tau-a*)))

    (setf (delta g) newd)))

(defun deep-shrink (f s g gamma_s convergence-p verbose)
  (let ((bbr (fk s 0)))
  (unless (dv (data s))
    (maybe-reshape s g f :force t))

  (let* ((dv (dv (data s)))
	 (n (dimension s))
	 (dmin (dmin (data s)))
	 (|dmin| (norm dmin))
	 (converged-p nil)
	 (l 1))	;; This does the same as in the article

    (loop :until (or (< (fk s 0) bbr)
		     (setf converged-p (funcall convergence-p s)))
	  :do (let ((fkt (* (expt gamma_s (floor l 2))
			    (expt -1 l))))

		(when verbose
		  (format t "Shrinking by factor: ~A~%" fkt))

		(when (= 0.0d0 fkt)
		  ;; You've got no simplex anymore
		  (setf *breakdown* t))

		(when (< (* (abs fkt) |dmin|)
			 (* (/ *smalllambda* 2)
			    (sqrt n)
			    (norm (delta g))))

		  (regrid g (xk s 0) dmin fkt))

		(setf (xk s 0) (aref dv 0)
		      (fk s 0) bbr)

		(loop for k from 1 below (length (x s)) do
		      (setf (xk s k) (restrict g
					       (v+w*c (aref dv 0)
							     (aref dv k) fkt))
			    (fk s k) (funcall f (xk s k))))

		(sort-simplex s)
		(incf l)))

    (setf (data s) nil)

    converged-p)))
      
(defun grnm-optimize (objective-function initial-guess &key
		      max-function-calls
		      (convergence-p (burmen-et-al-convergence-test))
		      verbose)

  (let ((simplex (if (typep initial-guess 'nm-simplex)
		     initial-guess
		     (default-initial-simplex initial-guess)))
	(fvcount 0)
	(gamma_s 0.5d0)
	(*breakdown* nil))
    
    (labels ((rigged-f (v)
	       (incf fvcount)
	       (funcall objective-function v))
	     (converged-p (s)
	       (or (funcall convergence-p s)
		   *breakdown*
		   (and max-function-calls (> fvcount max-function-calls)))))
      (when verbose
	(format t "Initial simplex: ~%~A~%---~%" simplex))
      
      (maybe-fill-simplex simplex #'rigged-f)
      
  (prog (failure xbest fbest pbest reshaped-p
 
	 (grid (make-instance 'grid
			      :z (xk simplex 0)
			      :delta
			      (make-array (dimension simplex)
					  :initial-element
					  (/ (loop :for i :from 1 :to
						   (dimension simplex)
						   :minimizing
						   (norm
						    (v+w*c
						     (xk simplex 0)
						     (xk simplex i) -1.0d0)))
					     10.0d0)))))
     

   iterate ;; 1.
   (setf (values simplex failure)
	 (nm-iteration-burmen-et-al simplex #'rigged-f grid
				    :verbose verbose))

   ;; Small variation. We test for convergence here too. Depending on
   ;; the convergence criterion, this might be spurious, so we have to
   ;; reshape, etc.
   (unless (or failure (converged-p simplex))
     (go iterate))

   ;; 2.
   (setf xbest (xk simplex 0)
	 fbest (fk simplex 0)
	 pbest (aref (pmap simplex) 0)
   
	 reshaped-p (maybe-reshape simplex grid #'rigged-f))
	

   ;; 4. Here we look at the pseudo expand point
   (let* ((pep (pseudopivot simplex))
	  (xx (v+w*c pep
		     (v+w*c pep (xk simplex 0) -1.0d0)
		     1.0d0)) ;; To do: clear this up
	  (fpep (rigged-f xx)))

     (if (<= fbest (min (fk simplex 0) fpep))
	 (go deep-shrink)
	 (when (< fpep fbest)
	   ;; There is something subtle here. The pseudo-expand point
	   ;; is supposed to substitute the old (xk 0), which might
	   ;; have changed *position* during reshape, because of the
	   ;; simplex being sorted.
	   ;;
	   ;; So it is not
	   ;;(improve simplex xx fpep)
	   ;; here, but instead

	   ;; Step 5.
	   (setf (aref (x simplex) pbest) xx
		 (aref (fx simplex) pbest) fpep
		 (data simplex) nil)

	   (sort-simplex simplex)

	   ;; Step 6.
	   (go iterate))))
   
   deep-shrink

   (unless
       (deep-shrink #'rigged-f simplex grid

		    ;;(+ 0.1d0 (random 0.8d0))
		    gamma_s

		    #'converged-p verbose)
     (go iterate))

   end)
  (values (xk simplex 0) (fk simplex 0) simplex fvcount))))


;; Some sample functions to play around.

(defun standard-quadratic (v)
  (loop for i from 0 below (length v)
	summing (expt (aref v i) 2)))

(defun rosenbrock (v)
  (let ((x (aref v 0))
	(y (aref v 1)))

    (+ (expt (- 1 x) 2)
       (* 100 (expt (- y (expt x 2)) 2)))))
