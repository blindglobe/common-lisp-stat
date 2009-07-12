;;; -*- mode: lisp -*-

;;; Time-stamp: <2009-07-12 07:44:29 tony>
;;; Creation:   <2008-09-08 08:06:30 tony>
;;; File:       TODO.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c) 2007-2008, AJ Rossini <blindglobe@gmail.com>.  BSD.
;;; Purpose: Stuff that needs to be made working sits inside the
;;;          progns... This file contains the current challenges to
;;;          solve, including a description of the setup and the work
;;;          to solve....
 
;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; SET UP

(in-package :cl-user)

(defun init-CLS ()
  (asdf:oos 'asdf:load-op 'lispstat)
  (asdf:oos 'asdf:load-op 'cl-pdf)
  (asdf:oos 'asdf:load-op 'cl-typesetting))

(defun init-CLS-graphics ()
  (init-CLS)
  (asdf:oos 'asdf:load-op 'cl-cairo2-x11)
  (asdf:oos 'asdf:load-op 'cl-2d))

(init-CLS-graphics)

;;(asdf:oos 'asdf:load-op 'lisp-matrix)
;;(asdf:oos 'asdf:compile-op 'lispstat :force t)
;;(asdf:oos 'asdf:load-op 'lispstat)


;;(asdf:oos 'asdf:load-op 'cl-pdf)
;;(asdf:oos 'asdf:load-op 'cl-typesetting)

(in-package :lisp-stat-unittests)

;; tests = 80, failures = 8, errors = 15
(run-tests :suite 'lisp-stat-ut)
(describe (run-tests :suite 'lisp-stat-ut))

(describe 'lisp-stat-ut)
(documentation 'lisp-stat-ut 'type)

;; FIXME: Example: currently not relevant, yet
;;   (describe (lift::run-test :test-case  'lisp-stat-unittests::create-proto
;;                             :suite 'lisp-stat-unittests::lisp-stat-ut-proto))

(describe (lift::run-tests :suite 'lisp-stat-ut-dataframe))
(lift::run-tests :suite 'lisp-stat-ut-dataframe)

(describe (lift::run-test
	   :test-case  'lisp-stat-unittests::create-proto
	   :suite 'lisp-stat-unittests::lisp-stat-ut-proto))

(in-package :ls-user)

;;; DSC2009 examples in ./examples/*.lisp


#+nil(progn
  ;; REVIEW: general Lisp use guidance

  (fdefinition 'make-matrix)
  (documentation 'make-matrix 'function)

#| Examples from CLHS, a bit of guidance.

  ;; This function assumes its callers have checked the types of the
  ;; arguments, and authorizes the compiler to build in that assumption.
  (defun discriminant (a b c)
   (declare (number a b c))
   "Compute the discriminant for a quadratic equation."
   (- (* b b) (* 4 a c))) =>  DISCRIMINANT
  (discriminant 1 2/3 -2) =>  76/9

  ;; This function assumes its callers have not checked the types of the
  ;; arguments, and performs explicit type checks before making any assumptions. 
  (defun careful-discriminant (a b c)
    "Compute the discriminant for a quadratic equation."
    (check-type a number)
    (check-type b number)
    (check-type c number)
    (locally (declare (number a b c))
      (- (* b b) (* 4 a c)))) =>  CAREFUL-DISCRIMINANT
  (careful-discriminant 1 2/3 -2) =>  76/9
|#
  )


#+nil
(progn
  (asdf:oos 'asdf:load-op 'versioned-objects)
  (asdf:oos 'asdf:load-op 'validations))




#|
  (with-data dataset ((dsvarname1 [usevarname1])
                      (dsvarname2 [usevarname2]))
      @body)
|#




(defun testme (&key (a 3) (b (+ a 3)))
  b)

(testme)
(testme :a 2)
(testme :b 4)
(testme :a 2 :b (* a 5))


;;;; CL-PDF / CL-TYPESETTING experiments

(in-package :cl-user)

;;(asdf:oos 'asdf:load-op 'cl-pdf)

;;(asdf:oos 'asdf:load-op 'cl-typesetting)

;;; cl-pdf copyright 2002-2005 Marc Battyani see license.txt for
;;; details of the BSD style license

;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(defun example1 (&optional (file #P"/tmp/ex1.pdf"))
  (with-open-file (outstream file
			     :direction :output
			     :if-exists :rename
			     :element-type :default)
    (pdf:with-document ()
      (pdf:with-page ()
	(pdf:with-outline-level ("Example" (pdf:register-page-reference))
	  (let ((helvetica (pdf:get-font "Helvetica")))
	    (pdf:in-text-mode
	      (pdf:set-font helvetica 36.0)
	      (pdf:move-text 100 800)
	      (pdf:draw-text "cl-pdf: Example 1"))
	    (pdf:translate 230 500)
	    (loop repeat 150
	       for i = 0.67 then (* i 1.045)
	       do (pdf:in-text-mode
		    (pdf:set-font helvetica i)
		    (pdf:set-rgb-fill (/ (random 255) 255.0) 
				      (/ (random 255) 255.0) 
				      (/ (random 255) 255.0))
		    (pdf:move-text (* i 3) 0)
		    (pdf:show-text "cl-typesetting"))
		 (pdf:rotate 13)))))
      (pdf:write-document outstream))))

(example1)

(with-open-file (tony #p"/tmp/tony.pdf"
		      :direction :output
		     ; :element-type 'signed-byte
		     ; :element-type :default
		      :if-exists :supersede )
  (example1 tony))

;; for the TrueType Example, you need to load the font first:
;; (read the unicode-readme for more info)
#+nil
(pdf:load-ttu-font #P"/tmp/times.ufm" #P"/tmp/times.ttf")

(defun example1-ttu (&optional (file #P"/tmp/ex1-ttu.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let ((helvetica (pdf:get-font "TimesNewRomanPSMT"))) ; The windows times font
	  (pdf:in-text-mode
	   (pdf:set-font helvetica 36.0)
	   (pdf:move-text 100 800)
	   (pdf:draw-text "cl-pdf: Example 1 with Unicode"))
	  (pdf:translate 230 500)
	  (loop repeat 150
	 for i = 0.67 then (* i 1.05)
	 do (pdf:in-text-mode
	     (pdf:set-font helvetica i)
	     (pdf:set-rgb-fill (/ (random 255) 255.0)(/ (random 255) 255.0)(/ (random 255) 255.0))
	     (pdf:move-text (* i 3) 0)
	     (pdf:show-text (format nil "Lisp lives! ~cx.~cy.x " (code-char 955)(code-char 955))))
	   (pdf:rotate 13)))))
    (pdf:write-document file)))

(defun example2 (&optional (file #P"/tmp/ex2.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let ((helvetica (pdf:get-font "Helvetica")))
	  (pdf:in-text-mode
	   (pdf:set-font helvetica 36.0)
	   (pdf:move-text 100 800)
	   (pdf:draw-text "cl-pdf: Example 2"))
	  (pdf:move-to (+ 10 (random 500))(+ 10 (random 400)))
	  (pdf:set-gray-fill 0.5)
	  (dotimes (i 50)
	    (pdf:line-to (+ 50 (random 500)) (+ 50 (random 400))))
	  (pdf:close-even-odd-fill-and-stroke)
	  (pdf:move-to (+ 50 (random 500))(+ 400 (random 400)))
	  (pdf:set-rgb-fill 0.5 0.5 0.8)
	  (pdf:set-rgb-stroke 0.9 0.5 0.1)
	  (dotimes (i 50)
	    (pdf:bezier2-to (+ 50 (random 500)) (+ 400 (random 400))
			    (+ 50 (random 500)) (+ 400 (random 400))))
	  (pdf:close-even-odd-fill-and-stroke))))
    (pdf:write-document file)))

(defun gen-image-bits ()
  (with-output-to-string (s)
     (loop for x from -10 to 10 by 1/10
	   do (loop for y from -10 to 10 by 1/10
		    do (format s "~2,'0x~2,'0x~2,'0x"
			  (round (+ 200 (* 55 (sin x))))
			  (round (+ 200 (* 55 (cos y))))
			  (round (+ 200 (* 55 (sin (+ x y))))))))))

(defun example3 (&optional (file #P"/tmp/ex3.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let* ((helvetica (pdf:get-font "Helvetica"))
	       (image (make-instance 'pdf:image
				     :bits (gen-image-bits) 
				     :width 201 :height 201)))
	  (pdf:draw-bar-code128 "30A0033111436" 20 100)
	  (pdf:add-images-to-page image)
	  (pdf:in-text-mode
	   (pdf:set-font helvetica 36.0)
	   (pdf:move-text 100 800)
	   (pdf:draw-text "cl-pdf: Example 3"))
	  (pdf:with-saved-state
	      (pdf:translate 102 550)
	    (pdf:rotate 20)
	    (pdf:scale 200 125)
	    (pdf:paint-image image))
	  (pdf:with-saved-state
	      (pdf:translate 100 540)
	    (pdf:rotate -70)
	    (pdf:scale 300 200)
	    (pdf:paint-image image)))))
    (pdf:write-document file)))

(example1)

;; logo

(defparameter *fractal-ratio* 0.8)
(defconstant +sin60+ (sin (/ pi 3)))
(defconstant +cos60+ (cos (/ pi 3)))
(defconstant +tg30+ (tan (/ pi 6)))
(defconstant +tg60-tg30+ (- (tan (/ pi 3))(tan (/ pi 6))))

(defun %fractal (x y dx dy level)
  (if (zerop level)
    (let ((dx/2 (* dx 0.5))
	  (dy/2 (* dy 0.5)))
      (pdf:move-to (- x dx/2) (- y dy/2))
      (pdf:line-to x (+ y dy/2))
      (pdf:line-to (+ x dx/2) (- y dy/2))
      (pdf:close-fill-and-stroke))
    (let* ((delta (- 1 *fractal-ratio*))
	   (delta05 (* 0.5 delta))
	   (ratio2 (- 1 delta05))
	   (deltax (* dx 0.25 (+ 1 (* 0.5 +sin60+ (- 1 ratio2)))))
	   (deltay (* dy 0.25 (+ 1 delta05)))
	   (dyf2 (* dy 0.5 (+ 1 delta05 )))
	   (dxf2 (* dx 0.5 (+ 1 delta05 ))))
      (decf level)
      (setf dx (* dx 0.5))
      (setf dy (* dy 0.5))
      (%down-fractal x (- y (* 1 dy)(* dx +tg30+ -1)(* 0.125 +tg60-tg30+ dxf2)) dxf2 dyf2 level)
      (%fractal x      (+ y (* dy 0.5)) (* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
      (%fractal (+ x deltax)(- y deltay)(* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
      (%fractal (- x deltax)(- y deltay)(* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
      )))

(defun %down-fractal (x y dx dy level)
  (setf level 0) 
  (if (zerop level)
    (let ((dx/2 (* dx 0.5))
	  (dy/2 (* dy 0.5)))
      (pdf:move-to (- x dx/2) (+ y dy/2))
      (pdf:line-to x (- y dy/2))
      (pdf:line-to (+ x dx/2)(+ y dy/2))
      (pdf:close-fill-and-stroke))
    (let* ((delta (- 1 *fractal-ratio*))
	   (delta05 (* 0.5 delta))
	   (ratio2 (- 1 delta05))
	   (deltax (* dx 0.25 (+ 1 (* 0.5 +sin60+ (- 1 ratio2)))))
	   (deltay (* dy 0.25 (+ 1 delta05)))
	   (dyf2 (* dy 0.5 (+ 1 delta05 )))
	   (dxf2 (* dx 0.5 (+ 1 delta05 ))))
      (decf level)
      (setf dx (* dx 0.5))
      (setf dy (* dy 0.5))
      (%fractal x (+ y (* 1 dy)(* dx +tg30+ -1)(* 0.125 +tg60-tg30+ dxf2)) dxf2 dyf2 level)
      (%down-fractal x      (- y (* dy 0.5)) (* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
      (%down-fractal (+ x deltax)(+ y deltay)(* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
      (%down-fractal (- x deltax)(+ y deltay)(* dx *fractal-ratio*) (* dy *fractal-ratio*) level)
      )
    ))

(defun fractal (x y l level)
  (let ((dx l)
	(dy (* l +sin60+)))
  (%fractal x y dx dy level)))

;the logo
(defun example4 (&optional (file #P"/tmp/ex4.pdf"))
  (pdf:with-document ()
    (loop for i from 1 to 7
	  do (pdf:with-page ()
	       (pdf:with-outline-level ((format nil "Page ~d" i)(pdf:register-page-reference))
		 (let* ((helvetica (pdf:get-font "Helvetica")))
		   (pdf:in-text-mode
		    (pdf:set-font helvetica 36.0)
		    (pdf:move-text 100 800)
		    (pdf:draw-text (format nil "cl-pdf: Example 4    page ~d" i)))
		   (pdf:set-rgb-stroke 1.0 1.0 1.0)
		   (pdf:set-rgb-fill 0.4 0.4 0.9)
		   (pdf:set-line-width 0.2)
		   (fractal 298 530 600 i)))))
    (pdf:write-document file)))

(defvar *dx* #(1 0 -1 0))
(defvar *dy* #(0 1 0 -1))

;make-maze
(defun example5 (nx ny &key (size 10) (file #P"/tmp/ex5.pdf"))
  (let ((x-stack '())
	(y-stack '())
	(visited (make-array (list nx ny) :initial-element nil))
	(v-walls (make-array (list nx ny) :initial-element t))
	(h-walls (make-array (list nx ny) :initial-element t))
	(x (random nx))
	(y (random ny))
	next-x next-y)
    (flet ((find-cell ()
	       (let ((tested (vector nil nil nil nil))
		     (nb-tested 0))
		 (loop while (< nb-tested 4)
		       for test = (random 4)
		       unless (svref tested test)
		       do (incf nb-tested)
		       (setf (svref tested test) t)
		       (setf next-x (+ x (svref *dx* test)))
		       (setf next-y (+ y (svref *dy* test)))
		       (when (and (>= next-x 0)(< next-x nx)(>= next-y 0)(< next-y ny)
				  (not (aref visited next-x next-y)))
			 (return-from find-cell t)))
		 nil)))
      (setf (aref visited x y) t)
      (loop with nb-visited = 1 and total-cells = (* nx ny)
	    while (< nb-visited total-cells)
	    do (if (find-cell)
		 (progn (push x x-stack)(push y y-stack)
			(if (/= next-x x)
			  (setf (aref h-walls (min x next-x) y) nil)
			  (setf (aref v-walls x (min y next-y)) nil))
			(setf x next-x y next-y)
			(setf (aref visited x y) t)
			(incf nb-visited))
		 (progn (setf x (pop x-stack) y (pop y-stack))))))
    (pdf:with-document ()
      (pdf:with-page ()
	(pdf:with-outline-level ("Example" (pdf:register-page-reference))
	  (pdf:translate (* 0.5 (- 595 (* nx size)))(* 0.5 (- 841 (* ny size))))
	  (setf (aref h-walls (1- nx) (random ny)) nil)
	  (pdf:move-to 0 0)
	  (pdf:line-to (*  nx size) 0)
	  (pdf:move-to 0 size)
	  (pdf:line-to 0 (* ny size))
	  (loop for x from 0 below nx
		for x0 = 0 then x1
		for x1 from size by size
		do (loop for y from 0 below ny
			 for y0 = 0 then y1
			 for y1 from size by size
			 do
			 (when (aref h-walls x y)
			   (pdf:move-to x1 y0)
			   (pdf:line-to x1 y1))
			 (when (aref v-walls x y)
			   (pdf:move-to x0 y1)
			   (pdf:line-to x1 y1)))
		(pdf:stroke))))
      (pdf:write-document file))))


(defun example6 (&optional (file #P"/tmp/ex6.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let ((helvetica (pdf:get-font "Helvetica")))
	  (pdf:in-text-mode
	   (pdf:set-font helvetica 36.0)
	   (pdf:move-text 100 800)
	   (pdf:draw-text "cl-pdf: Example 6"))
	  (pdf:set-rgb-stroke 0.1 0.1 0.1)
	  (pdf:set-rgb-fill 0.8 0.8 0.8)
	  (let ((x 50) (y 600))
	    (dotimes (i 2)
	      (pdf:rectangle x y 500 140 :radius 10)
	      (pdf:close-fill-and-stroke)
	      (setf y (- y 180))))
	  (pdf:translate 50 670)
	  (let ((x 50) (y 0))
	    (loop repeat 4
	      for i = 8 then (* i 1.05)
	      do
	      (pdf:set-rgb-fill (* 0.1 i) (* 0.01 i) (* 0.02 i))
	      (pdf:circle x y (* 4 i))
	      (pdf:close-fill-and-stroke)
	      (pdf:ellipse (+ x 250) y (* 5 i) (* 4 i))
	      (pdf:close-fill-and-stroke)
	      (setf x (+ x 50))))
	  (pdf:translate 0 -180)
	  (pdf:regular-polygon 150 0 50 7 :fillet-radius 8)
	  (pdf:close-fill-and-stroke)
	  (pdf:star 350 0 50 30 6 :fillet-radius 5)
	  (pdf:close-fill-and-stroke)
	  
	  (pdf:set-rgb-fill 0.8 0.6 0.2)
	  (pdf:regular-polygon 150 0 30 5 :fillet-radius 4)
	  (pdf:close-fill-and-stroke)
	  (pdf:star 350 0 40 20 4 :fillet-radius 6)
	  (pdf:close-fill-and-stroke)
	  
	  (pdf:set-rgb-fill 0.4 0.8 0.7)
	  (pdf:regular-polygon 150 0 15 3 :fillet-radius 3)
	  (pdf:close-fill-and-stroke)
	  (pdf:star 350 0 35 10 12 :fillet-radius 1)
	  (pdf:close-fill-and-stroke)
	  (pdf:set-line-width 0.5)
	  (loop for r from 2 to 100 by 2
		for start = (* pi 0.001 (random 2000))
		for length = (* pi 0.001 (random 2000))
		do (pdf:set-rgb-stroke (* 0.01 (random 100))(* 0.01 (random 100))(* 0.01 (random 100)))
		(pdf:arc 250 -230 r start length)
		(pdf:stroke)))))
    (pdf:write-document file)))

(defvar *test-jpeg-file-path* (when *load-pathname*
			   (merge-pathnames #P"banner.jpg" *load-pathname*)))

(unless *test-jpeg-file-path*
  (error "please set the *test-jpeg-file-path* variable to the banner.jpg file location"))

(defvar *test-jpeg* *test-jpeg-file-path*)

(defun example7 (&optional (file #P"/tmp/ex7.pdf"))
  (pdf:with-document ()
    (let ((jpg-image (pdf:make-jpeg-image *test-jpeg*))
	  (helvetica (pdf:get-font "Helvetica")))
      (pdf:with-outline-level ("Contents" "page 1")
	(pdf:with-page ()
	  (pdf:register-page-reference "page 1")
	  (pdf:with-outline-level ("Page 1" "page 1")
	    (pdf:in-text-mode
	     (pdf:set-font helvetica 36.0)
	     (pdf:move-text 100 800)
	     (pdf:draw-text "cl-pdf: Example 7"))
	    (pdf:set-rgb-stroke 0.1 0.1 0.1)
	    (pdf:set-rgb-fill 0.6 0.6 0.8)
	    (pdf:in-text-mode
	     (pdf:set-font helvetica 13.0)
	     (pdf:move-text 10 700)
	     (pdf:draw-text "Test for bookmarks, JPEG support, internal links, URI links and basic charts"))
	    (pdf:add-images-to-page jpg-image)
	    (pdf:draw-image jpg-image 10 10 239 50 0 t)
	    (pdf:add-URI-link 10 10 239 50 "http://www.fractalconcept.com/asp/html/cl-pdf.html" :border #(1 1 1))
	    (pdf:in-text-mode
	     (pdf:set-font helvetica 10.0)
	     (pdf:move-text 500 10)
	     (pdf:draw-text "goto page 2"))
	    (pdf:add-link 495 8 80 14 "page 2")
	    (pdf:draw-object (make-instance 'pdf:histogram :x 200 :y 450 :width 300 :height 200
					    :label-names '("Winter" "Spring" "Summer" "Autumn")
					    :labels&colors '(("Serie 1" (1.0 0.0 0.0))
							     ("Serie 2" (0.0 1.0 0.0)))
					    :series '((42 46 48 42)(40 38 51 46))
					    :background-color '(0.9 0.9 0.9)
					    :stacked-series nil ;;; try also with t
					    :x-axis-options ()
					    :y-axis-options ()
					    :legend-options ()))
	    (pdf:draw-object (make-instance 'pdf:pie-chart :x 200 :y 100 :width 200 :height 200
					    :serie '(12 23 65 33)
					    :labels&colors '(("Winter" (1.0 0.0 0.0))
							     ("Spring" (0.0 1.0 0.0))
							     ("Summer" (0.0 0.0 1.0))
							     ("Autumn" (0.0 1.0 1.0)))))))
	(pdf:with-page ()
	  (pdf:register-page-reference "page 2")
	  (pdf:with-outline-level ("Page 2" "page 2")
	    (pdf:in-text-mode
	     (pdf:set-font helvetica 36.0)
	     (pdf:move-text 100 800)
	     (pdf:draw-text "Page 2"))
	    (pdf:add-images-to-page jpg-image)
	    (pdf:draw-image jpg-image 10 10 239 50 0 t)
	    (pdf:add-URI-link 10 10 239 50
			      "http://www.fractalconcept.com/asp/html/cl-pdf.html"
			      :border #(1 1 1))
	    (pdf:in-text-mode
	     (pdf:set-font helvetica 10.0)
	     (pdf:move-text 500 10)
	     (pdf:draw-text "goto page 1"))
	    (pdf:add-link 495 8 80 14 "page 1")
	    (pdf:draw-object 
	     (make-instance 'pdf:plot-xy :x 100 :y 400 :width 400 :height 200
			    :labels&colors '(("Data 1" (1.0 0.0 0.0))
					     ("Data 2" (0.0 1.0 0.0))
					     ("Data 3" (0.0 0.0 1.0)))
			    :series '(((1 40) (3 38) (5 31) (7 36))
				      ((2 53) (2.5 42) (3.7 46) (6 48))
				      ((1.3 12) (1.6 18) (2 16) (3 27)))
			    :background-color '(0.9 0.9 0.9)
			    :x-axis-options ()
			    :y-axis-options '(:min-value 0)
			    :legend-options ()))))))
    (pdf:write-document file)))

; Von Koch fractal (brute force ;-))

(defun vk-fractal (l level)
  (pdf:with-saved-state
      (if (zerop level)
          (progn 
            (pdf:move-to 0 0)
            (pdf:line-to l 0)
            (pdf:stroke))
          (loop with l3 = (/ l 3.0) and l-1 = (1- level)
                for angle in '(nil 60 -120 60)
                do (when angle (pdf:rotate angle))
                   (vk-fractal l3 l-1))))
  (pdf:translate l 0))


(defun example8 (&optional (file #P"/tmp/ex8.pdf"))
  (pdf:with-document ()
    (loop for i from 0 to 6
	  do (pdf:with-page ()
	       (pdf:with-outline-level ((format nil "Page ~d" i)(pdf:register-page-reference))
		 (let* ((helvetica (pdf:get-font "Helvetica" :win-ansi-encoding)))
		   (pdf:draw-centered-text
		    297 800
		    (format nil "Flocon de Koch (niveau ~d, ~d segments, périmètre ~,1f mm)" 
			    i (* 3 (expt 4 i))(/ (* 180 (* 3 (expt 4 i)))(expt 3 i)))
				      helvetica 18.0)
                   (pdf:translate 42 530)
		   (pdf:set-line-width 0.1)
		   (vk-fractal 510 i)
		   (pdf:rotate -120)
		   (vk-fractal 510 i)
		   (pdf:rotate -120)
		   (vk-fractal 510 i)))))
    (pdf:write-document file)))

;;; A Mandelbrot set from Yannick Gingras

(defun hsv->rgb (h s v)
  "channels are in range [0..1]"
  (if (eql 0 s)
      (list v v v)
      (let* ((i (floor (* h 6)))
             (f (- (* h 6) i))
             (p (* v (- 1 s)))
             (q (* v (- 1 (* s f))))
             (t_ (* v (- 1 (* s (- 1 f)))))
             (hint (mod i 6)))
        (case hint
          (0 (list v t_ p))
          (1 (list q v p))
          (2 (list p v t_))
          (3 (list p q v))
          (4 (list t_ p v))
          (5 (list v p q))))))

(defun make-color-map (nb-col
		       start-rad
		       &optional
		       stop-rad
		       (sat .85)
		       (tilt-angle 0)
		       (nb-loop 1)
		       (clockwise t))
  ;; borowed from Poly-pen --YGingras
  (let* ((stop-rad (if stop-rad stop-rad start-rad))
         (angle-inc (* (if clockwise 1.0 -1.0) nb-loop))
         (val-inc (- stop-rad start-rad)))
    (coerce (loop for k from 0 to (1- nb-col) collect
		  (let ((i (/ k (1- nb-col))))
		    (mapcar #'(lambda (x) (round x 1/255))
			    (hsv->rgb (mod (+ tilt-angle (* i angle-inc)) 1)
				      sat
				      (+ start-rad (* i val-inc))))))
            'vector)))


(defun gen-mandelbrot-bits (w h)
  ;; Inside a Moth by Lahvak, for other interesting regions see 
  ;;     http://fract.ygingras.net/top

  ;; TODO:AA
  (declare (optimize speed (debug 0) (safety 0) (space 0))
	   (type fixnum w h))
  (let* ((nb-cols 30000)
	 (nb-iter (expt 2 11)) ;; crank this if you have a fast box
	 (center #c(-0.7714390420105d0 0.1264514778485d0))
	 (zoom (* (expt (/ h 320) 2) 268436766))
	 (inc (/ h (* 150000d0 zoom)))
	 (cols (make-color-map nb-cols 1 0.2 0.9 0.24 0.21 t))
	 (c #c(0d0 0d0))
	 (z #c(0d0 0d0))
	 (region nil)) 
    (declare (type double-float inc))
    (dotimes (i h)
      (dotimes (j w)
	(setf c (complex (+ (realpart center) 
			    (* inc (+ (the fixnum j) (/ w -2d0))))
			 (+ (imagpart center) 
			    (* inc (+ (the fixnum i) (/ h -2d0)))))
	      z #c(0d0 0d0))
	;; standard Mandelbrot Set formula
	(push (dotimes (n nb-iter 0)
		(setf z (+ (* z z) c))
		(let ((real (realpart z))
		      (imag (imagpart z)))
		  (when (< 2 (abs z))
		    (return (- nb-iter 
			       ;; sub-iter smoothing
			       (- n (log (log (abs (+ (* z z) c)) 10) 2)))))))
	      region)))
    (with-output-to-string (s)
      (let ((max (reduce #'max region)))
	(dolist (x (nreverse region))
	  (destructuring-bind (r g b) 
	      (if (zerop x)
		  '(0 0 0)
		  ;; pallette stretching
		  (elt cols (floor (expt (/ x max) (/ nb-iter 256)) 
				   (/ 1 (1- nb-cols)))))
	    (format s "~2,'0x~2,'0x~2,'0x" r g b)))))))

;;; Example 9 is a Mandelbrot set from Yannick Gingras
;;; Takes a long time...

(defun example9 (&optional (file #P"/tmp/ex9.pdf"))
  "draw a nice region of the Mandelbrot Set"
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(let* ((w 600)
	       (h 750)
	       (helvetica (pdf:get-font "Helvetica"))
	       (image (make-instance 'pdf:image
				     :bits (gen-mandelbrot-bits w h) 
				     :width w :height h)))
	  (pdf:add-images-to-page image)
	  (pdf:in-text-mode
	    (pdf:set-font helvetica 36.0)
	    (pdf:move-text 100 800)
	    (pdf:draw-text "cl-pdf: Example 9"))
	  (pdf:with-saved-state
	      (pdf:translate 0 0)
	    (pdf:scale (/ w 2) (/ h 2))
	    (pdf:paint-image image)))))
    (pdf:write-document file)))


;; CL-TYPESETTING example

;;; cl-typesetting copyright 2003-2004 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-typesetting is here: http://www.fractalconcept.com/asp/html/cl-typesetting.html

(in-package typeset)

;;; Low level tests

(defparameter *boxes* nil) ;for debugging...

; a very simple hello world
(defun hello (&optional (file #P"/tmp/hello.pdf"))
    (with-open-file (outstream file
			     :direction :output
			     :if-exists :rename
			     :element-type :default)

  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Example" (pdf:register-page-reference))
	(pdf:set-line-width 0.1)
	(let ((content
	       (compile-text ()
			     (vspace 100)
		 (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 50 :color '(0.0 0 0.8))
			    "cl-typesetting" :eol
			    (vspace 2)
			    (hrule :dy 1)
			    (with-style (:font "Times-Italic" :font-size 26)
			      "The cool Common Lisp typesetting system")
			    (vspace 50)
			    (with-style (:font "Helvetica-Oblique" :font-size 100)
			      "Hello World!")
			    (vspace 50)
			    (with-style (:font "Helvetica" :font-size 12)
			      "hello" (dotted-hfill) "4.2" :eol
			      "hello world" (dotted-hfill) "4.2.4.2"))
		 (paragraph (:h-align :justified :font "Helvetica" :font-size 12 :color '(0.0 0 0.0))
			    "hello" (dotted-hfill) "4.2" :eol
			    "hello world" (dotted-hfill) "4.2.4.2")
			    (vspace 50)
		 (paragraph (:h-align :justified :font "Helvetica" :font-size 12 :color '(0.0 0 0.0))
			    "hello" (dotted-hfill :pattern-spacing 0.6) "4.2" :eol
			    "hello world" (dotted-hfill :pattern-spacing 0.6) "4.2.4.2")
			    (vspace 50)
		 (paragraph (:h-align :justified :font "Helvetica" :font-size 12 :color '(0.0 0 0.0))
			    "hello" (dotted-hfill :char-pattern ".+" :pattern-spacing 0) "4.2" :eol
			    "hello world" (dotted-hfill :char-pattern ".+" :pattern-spacing 0) "4.2.4.2"))))
	  (draw-block content 20 800 545 700))))
    (pdf:write-document outstream))))

;;(hello)

; a multipage simple hello world
(defun multi-page-hello (&optional (file #P"/tmp/hello.pdf"))
    (with-open-file (outstream file
			     :direction :output
			     :if-exists :rename
			     :element-type :default)
  (pdf:with-document ()
    (let ((content
	   (compile-text ()
		(vspace 100)
		(paragraph (:h-align :center :font "Helvetica-Bold" :font-size 50 :color '(0.0 0 0.8))
			   "cl-typesetting" :eol
			   (vspace 2)
			   (hrule :dy 1)
			   (with-style (:font "Times-Italic" :font-size 26)
			     "The cool Common Lisp typesetting system")
			   (vspace 50)
			   (with-style (:font "Times-Italic" :font-size 36 :color '(0.0 0 0.8))
			     (dotimes (i 100)
			       (put-string "Hello World!")(new-line)))))))
      (loop while (boxes content) do
	    (pdf:with-page ()
	      (pdf:set-line-width 0.1)
	      (draw-block content 20 800 545 700))))
    (pdf:write-document outstream))))

;; (multi-page-hello)

;; The Fancy Example!

(defparameter *par1*
  "Lisp is a family of languages with a long history. Early key ideas in Lisp were developed by John McCarthy during the 1956 Dartmouth Summer Research Project on Artificial Intelligence. McCarthy's motivation was to develop an algebraic list processing language for artificial intelligence work. Implementation efforts for early dialects of Lisp were undertaken on the IBM 704, the IBM 7090, the Digital Equipment Corporation (DEC) PDP-1, the DEC PDP-6, and the PDP-10. The primary dialect of Lisp between 1960 and 1965 was Lisp 1.5. By the early 1970's there were two predominant dialects of Lisp, both arising from these early efforts: MacLisp and Interlisp. For further information about very early Lisp dialects, see The Anatomy of Lisp or Lisp 1.5 Programmer's Manual.")

(defparameter *par2*
  "MacLisp improved on the Lisp 1.5 notion of special variables and error handling. MacLisp also introduced the concept of functions that could take a variable number of arguments, macros, arrays, non-local dynamic exits, fast arithmetic, the first good Lisp compiler, and an emphasis on execution speed. By the end of the 1970's, MacLisp was in use at over 50 sites. For further information about Maclisp, see Maclisp Reference Manual, Revision 0 or The Revised Maclisp Manual.")

;; example of extension

(defclass rotated-char-box (soft-box h-mode-mixin)
  ((boxed-char :accessor boxed-char :initarg :boxed-char)
   (rotation :accessor rotation :initarg :rotation)))

(defun put-rotated-char-string (string)
  (loop for char across string
	do (add-box (make-instance 'rotated-char-box :dx *font-size*
				   :dy *font-size* :boxed-char char :baseline (* *font-size* 0.8)
				   :rotation (- (random 120) 60)))))

(defmethod stroke ((box rotated-char-box) x y)
  (let ((dx (dx box))(dy (dy box))
	(width (pdf:get-char-width (boxed-char box) *font* *font-size*)))
    (pdf:with-saved-state
      (pdf:translate (+ x (* dx 0.5)) (+ y (* dy 0.3)))
      (pdf:set-line-width 0.5)
      (pdf:set-gray-fill 0.8)
      (pdf:circle 0 0 (* dx 0.45))
      (pdf:fill-and-stroke)
      (pdf:set-gray-fill 0)
      (pdf:rotate (rotation box))
      (pdf:in-text-mode
       (pdf:move-text (* -0.5 width)(* -0.18 *font-size*))
       (pdf:set-font *font* (* *font-size* 0.8))
       (pdf:show-text (make-string 1 :initial-element (boxed-char box)))))))

;; a draw function for the functional rule...

(defun draw-wavelet-rule (box x0 y0)
  (let ((dx/2 (* (dx box) 0.5))
	(dy/2 (* (dy box) 0.5)))
    (pdf:with-saved-state
      (pdf:translate (+ x0 dx/2) (- y0 dy/2))
      (pdf:set-line-width 1)
      (pdf:set-color-stroke (color box))
      (pdf:move-to (- dx/2) 0)
      (loop with a = (/ -0.2 (dx box)) and w = (/ 200.0 (dx box))
	    for x from (- dx/2) by 0.2
	    for y = (* dy/2 (cos (* x w)) (exp (* x x a)))
	    while (< x dx/2)
	    do (pdf:line-to x y))
      (pdf:stroke))))

;; user-drawn box

(defun user-drawn-demo (box x y)
  (draw-block
   (compile-text ()
     (paragraph (:h-align :justified :top-margin 5 :first-line-indent 10
			  :font "Times-Italic" :font-size 6.5)
       *par1*))
   x (- y (dy box)) (- (dy box) 10) (dx box) :rotation 90 :border 0.1))

;; a chart (I will have to change this in cl-pdf: it's a real mess!)

(defun draw-pie (box x y)
  (pdf:draw-object (make-instance
		    'pdf:pie-chart :x (+ x 30) :y (- y 100) :width 90 :height 90
		    :serie '(12 23 65 33)
		    :labels&colors
		    '(("Winter" (1.0 0.0 0.0))
		      ("Spring" (0.0 1.0 0.0))
		      ("Summer" (0.0 0.0 1.0))
		      ("Autumn" (0.0 1.0 1.0))))))

;; a stupid trick
;; brute force!!!
(defun link-all-a (box x y)
  (pdf:with-saved-state
      (let ((all-a ()))
	(map-boxes box 0 0
		   #'(lambda (box x y)
		       (when (and (char-box-p box) (char= (boxed-char box)#\a))
			 (push (list (+ x (* 0.5 (dx box)))
				     (+ y (* 0.2 (dy box)))
				     box)
			       all-a))))
	(pdf:set-line-width 1)
	(pdf:set-rgb-stroke 1.0 0.8 0.4)
	(loop for (x y box) in all-a
	      for sorted-a = (sort (copy-seq all-a)
				   #'(lambda (item1 item2)
				       (let ((dx1 (- (first item1) x))
					     (dy1 (- (second item1) y))
					     (dx2 (- (first item2) x))
					     (dy2 (- (second item2) y)))
					 (<= (sqrt (+ (* dx1 dx1)
						      (* dy1 dy1)))
					     (sqrt (+ (* dx2 dx2)
						      (* dy2 dy2)))))))
	      do (loop repeat 4
		   for (x2 y2 box) in sorted-a
		   do
	         (pdf:set-gray-fill 0.8)
		   (pdf::move-to x y)
		   (pdf::line-to x2 y2)
		   (pdf::stroke)))
	(pdf:set-gray-fill 0.8)
	(pdf:set-rgb-stroke 0.5 0.7 1.0)
	(loop for (x y box) in all-a
	      do
	      (pdf:circle x y (* (dx box) 0.7))
	      (pdf:fill-and-stroke)))))

;;; rivers detection (still brute force not to be used in real life...)
(defun link-all-spaces (box x y)
  (pdf:with-saved-state
      (let ((all-spaces ()))
	(map-boxes box 0 0
		   #'(lambda (box x y)
		       (when (white-char-box-p box)
			 (push (list (+ x (* 0.5 (dx box)))
				     (+ y (* 0.5 (dx box)))
				     box)
			       all-spaces))))
	(pdf:set-line-width 1)
	(pdf:set-rgb-stroke 1.0 0.4 0)
	(loop for (x y box) in all-spaces
	      for sorted-spaces = (sort (copy-seq all-spaces)
					#'(lambda (item1 item2)
					    (let ((dx1 (- (first item1) x))
						  (dx2 (- (first item2) x)))
					      (<= (abs dx1)(abs dx2)))))
	      do (loop repeat 5
		   for (x2 y2 box) in sorted-spaces
		   for dy = (abs (- y2 y))
		   for dx = (abs (- x2 x))
		   do
		   (when (and (< dx (* 1.5 (+ (dx box)
					      (delta-size box))))
			      (< 0 dy (* 5 (dx box))))
		     (pdf:set-gray-fill 0.8)
		     (pdf::move-to x y)
		     (pdf::line-to x2 y2)
		     (pdf::stroke)
		     (pdf:circle x y (* (dx box) 0.7))
		     (pdf:circle x2 y2 (* (dx box) 0.7))
		     (pdf:fill-and-stroke)))))))

(defun link-all-a-and-spaces (box x y)
  (link-all-a box x y)
  (link-all-spaces box x y))

(defun gen-box-graph ()
  (let* ((g1 (make-instance 'graph :dot-attributes '(#+nil("rankdir" "LR")("nodesep" "0.3")("ranksep" "0.8"))
			    :max-dx 500 :max-dy 300 :border-width nil))
	 (n1 (make-instance 'graph-node :data "box" :graph g1))
	 (n2 (make-instance 'graph-node :data "h-mode-mixin" :graph g1))
	 (n3 (make-instance 'graph-node :data "v-mode-mixin" :graph g1))
	 (n5 (make-instance 'graph-node :data "soft-box" :graph g1))
	 (n6 (make-instance 'graph-node :data "container-box" :graph g1))
	 (n7 (make-instance 'graph-node :data "vbox" :graph g1))
	 (n8 (make-instance 'graph-node :data "hbox" :graph g1))
	 (n9 (make-instance 'graph-node :data "glue" :graph g1))
	 (n10 (make-instance 'graph-node :data "hglue" :graph g1))
	 (n11 (make-instance 'graph-node :data "vglue" :graph g1))
	 (n12 (make-instance 'graph-node :data "spacing" :graph g1))
	 (n13 (make-instance 'graph-node :data "h-spacing" :graph g1))
	 (n14 (make-instance 'graph-node :data "v-spacing" :graph g1))
	 (n15 (make-instance 'graph-node :data "char-box" :graph g1))
	 (n16 (make-instance 'graph-node :data "white-char-box" :graph g1)))
    (add-rank-constraint g1 "same" (list n1 n5 n15))
    (make-instance 'graph-edge :head n1 :tail n5 :label "subclass" :graph g1)
    (make-instance 'graph-edge :head n5 :tail n6 :graph g1)
    (make-instance 'graph-edge :head n6 :tail n7 :graph g1)
    (make-instance 'graph-edge :head n2 :tail n7 :graph g1)
    (make-instance 'graph-edge :head n6 :tail n8 :graph g1)
    (make-instance 'graph-edge :head n3 :tail n8 :graph g1)
    (make-instance 'graph-edge :head n5 :tail n9 :graph g1)
    (make-instance 'graph-edge :head n9 :tail n10 :graph g1)
    (make-instance 'graph-edge :head n2 :tail n10 :graph g1)
    (make-instance 'graph-edge :head n9 :tail n11 :graph g1)
    (make-instance 'graph-edge :head n3 :tail n11 :graph g1)
    (make-instance 'graph-edge :head n5 :tail n12 :graph g1)
    (make-instance 'graph-edge :head n12 :tail n13 :graph g1)
    (make-instance 'graph-edge :head n2 :tail n13 :graph g1)
    (make-instance 'graph-edge :head n12 :tail n14 :graph g1)
    (make-instance 'graph-edge :head n3 :tail n14 :graph g1)
    (make-instance 'graph-edge :head n1 :tail n15 :graph g1)
    (make-instance 'graph-edge :head n2 :tail n15 :graph g1)
    (make-instance 'graph-edge :head n10 :tail n16 :graph g1)
    (compute-graph-layout g1)
    g1))

(defun make-color-node-box (graph name color description)
  (make-instance 'graph-node :graph graph :dx 70 :data
		 (make-filled-vbox
		  (compile-text ()
 		     (paragraph (:h-align :center :font "Helvetica-Oblique"
					  :font-size 14 :color '(0 0 0))
				(put-string name) " "
				(colored-box :dx 9.0 :dy 9.0 :color color :border-width 0.5)
				:eol
				(with-style (:font "Times-Italic" :font-size 10)
				  (put-string description))))
		  70 +huge-number+)))

(defun gen-color-graph ()
  (let* ((g1 (make-instance 'graph :dot-attributes '(#+nil("rankdir" "LR")("nodesep" "0.3")("ranksep" "0.8"))
			    :max-dx 500 :max-dy 300 :border-width nil))
	 (red (make-color-node-box g1 "red" '(1.0 0 0) "(primary color)"))
	 (green (make-color-node-box g1 "green" '(0 1.0 0) "(primary color)"))
	 (blue (make-color-node-box g1 "blue" '(0 0 1.0) "(primary color)"))
	 (cyan (make-color-node-box g1 "cyan" '(0 1.0 1.0) "(green + blue)"))
	 (magenta (make-color-node-box g1 "magenta" '(1.0 0 1.0) "(red + blue)"))
	 (yellow (make-color-node-box g1 "yellow" '(1.0 1.0 0) "(red + green)")))
    (make-instance 'graph-edge :head blue :tail cyan :graph g1)
    (make-instance 'graph-edge :head green :tail cyan :graph g1)
    (make-instance 'graph-edge :head red :tail magenta :graph g1)
    (make-instance 'graph-edge :head blue :tail magenta :graph g1)
    (make-instance 'graph-edge :head red :tail yellow :graph g1)
    (make-instance 'graph-edge :head green :tail yellow :graph g1)
    (compute-graph-layout g1)
    g1))

;;; Example document
; Copy the files from the directory "files-for-example/" (included in
; the cl-typesetting distribution) to the /tmp directory (or somewhere else).
;
; Then you need to load the fonts with something like this:
;   (pdf:load-t1-font "/tmp/cmex10.afm" "/tmp/cmex10.pfb")
;   (pdf:load-t1-font "/tmp/cmti10.afm" "/tmp/cmti10.pfb")

(defun full-example (&key (file #P"/tmp/ex.pdf")
                     (banner-jpg #P"/tmp/banner.jpg")
                     (fractal-jpg #P"/tmp/fractal.jpg")
                     display-graphs)
  (with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Table of Content" (pdf:register-page-reference))
	(let ((content
	       (compile-text ()
			     (vspace 100)
			     (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 60 :color '(0.0 0 0.8))
					"cl-typesetting" :eol
					(vspace 2)
					(hrule :dy 1)
					(with-style (:font "Times-Italic" :font-size 28)
					  "The Cool Common Lisp Typesetting System"))
			     (vspace 100)
			     (hrule :dy 30 :stroke-fn 'draw-wavelet-rule :color '(0.0 0 0.6))
			     (vspace 250)
			     (hrule :dy 0.5 :color '(0.0 0 0.6))
			     (paragraph (:h-align :center :font "Helvetica" :font-size 16 :color '(0.0 0 0.6))
					"Table of content")
			     (hrule :dy 0.1 :color '(0.0 0 0.6))
			     (paragraph (:h-align :fill :font "Helvetica" :font-size 14 :color '(0.0 0 0.4)
						  :left-margin 25 :right-margin 25)
					"Hello world" (dotted-hfill)
					(format-string "~d" (find-ref-point-page-number "hello")) :eol
					"Full demo" (dotted-hfill)
					(format-string "~d" (find-ref-point-page-number "demo")) :eol
					"cl-typegraph" (dotted-hfill)
					(format-string "~d" (find-ref-point-page-number "graph")) :eol)
			     (vspace 2)
			     (hrule :dy 0.5 :color '(0.0 0 0.6)))))
	  (draw-block content 20 800 545 700))))
    (pdf:with-page ()
      (pdf:with-outline-level ("Hello world" (pdf:register-page-reference))
	(let ((content
	       (compile-text ()
	          (mark-ref-point "hello")
		  (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 30 :color '(0.0 0 0.8))
			     "cl-typesetting" :eol
			     (vspace 2)
			     (hrule :dy 1)
			     (with-style (:font "Times-Italic" :font-size 24)
			       "The Cool Common Lisp Typesetting System")
;			     (vspace 50)
;			     (with-style (:font "Helvetica-Oblique" :font-size 100)
;			       "Hello World!")
			     :vfill
			     (with-style (:font "Times-Italic" :font-size 100)
			       "Hello World!")
			     :vfill))))
	  (draw-block content 20 800 545 700))))
   (pdf:with-page ()
      (pdf:with-outline-level ("Full demo" (pdf:register-page-reference))
	(let ((content
	       (compile-text ()
	         (mark-ref-point "demo")
		 (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 30 :color '(0.0 0 0.8))
			    "cl-typesetting" :eol
			    (vspace 2)
			    (hrule :dy 1)
			    (with-style (:font "Times-Italic" :font-size 13)
			      "The Cool Common Lisp Typesetting System"))
		 (paragraph (:h-align :justified :top-margin 10 :first-line-indent 10
				      :font "Times-Italic" :font-size 9)
			      "This typesetting system's goal is to be an alternative to the TeX typesetting system. It is written in Common Lisp and uses cl-pdf as its backend. This enables it to be powerful, extensible, programmable  and fast. Though it is not considered very difficult, it is already much better than Word...")
		 (paragraph (:h-align :center :font "Helvetica-BoldOblique" :font-size 20 :color '(1.0 0 0))
			    "Now in Color! "
			    (colored-box :dx 15.0 :dy 15.0 :color "#FFC0C0" :border-width 0.5) " "
			    (colored-box :dx 15.0 :dy 15.0 :color "#C0FFC0" :border-width 0.5) " "
			    (colored-box :dx 15.0 :dy 15.0 :color "#C0C0FF" :border-width 0.5))
		 (paragraph (:h-align :center :font "Times-Italic" :font-size 12 :color '(0.0 0.6 0.3))
			    "With user defined "
			    (put-rotated-char-string "extensions") :eol
			    (with-style (:font "Times-Italic" :font-size 11)
			      "Support for images and functional rules" :eol
			      (image :file banner-jpg :dx 100 :dy 20)))
		 (hrule :dy 15 :stroke-fn 'draw-wavelet-rule)
		 (vspace 3)
		 (table (:col-widths '(60 80 80) :border 0.5 :background-color '(1 1 0.8)
				     :cell-padding 1 :padding 2)
			(row ()
			     (cell (:background-color '(0.8 1 0.8) :col-span 3)
				   (paragraph (:h-align :center :font "Times-Italic" :font-size 12)
						"Title with a col-span of 3")))
			(row ()
			     (cell (:background-color '(0.8 0.8 0.8))
				   (paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"Left aligned"))
			     (cell (:background-color '(0.8 0.8 0.8))
				   (paragraph (:h-align :center :font "Times-Roman" :font-size 9)
						"Centered cell content"))
			     (cell (:background-color '(0.8 0.8 0.8))
				   (paragraph (:h-align :right :font "Times-Bold" :font-size 9)
						"Right cell content")))
			(row ()
			     (cell ()(paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"This cell content should take three lines."))
			     (cell (:background-color '(1 1 1))
				   (paragraph (:h-align :center :font "Times-Italic" :font-size 9)
						"A jpeg "
						(image :file fractal-jpg :dx 15 :dy 15 :inline t
						       :offset 9)
						" in the text"))
			     (cell ()(paragraph (:h-align :left :font "Times-Italic" :font-size 11)
						(put-rotated-char-string "common lisp is cool"))))
			(row ()
			     (cell ()(paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"An example of table inside a cell"))
			     (cell (:background-color '(1 1 1))
				   (table (:col-widths '(14 14 21) :border 0.2
							 :background-color '(0.4 0.4 0.8))
					    (row () (cell () "12")(cell () "34")(cell () "567"))
					    (row () (cell () "ab")(cell () "cd")(cell () "efg"))))
			     (cell (:v-align :bottom)(paragraph (:h-align :left :font "Times-Italic" :font-size 9)
						"You can nest as many tables as you want, like you do in HTML."))))
		 (paragraph (:h-align :justified :top-margin 5 :first-line-indent 10 :color '(0 0 0)
				      :left-margin 5 :right-margin 5 
				      :font "Times-Roman" :font-size 10 :text-x-scale 0.7)
			    (with-style (:color '(0 0.6 0.4))
			      "This paragraph has been horizontally strechted by a 0.7 ratio. ")
			    *par1*)
		 (vspace 10)
		 (user-drawn-box :dx 210 :dy 100 :stroke-fn 'user-drawn-demo) :eol
		 (paragraph (:h-align :center :font "Times-Italic" :font-size 8 :top-margin 5)
			    "An example of using cl-typesetting in an user-drawn box.")
		 (paragraph (:h-align :left :top-margin 15
				      :left-margin 5 :right-margin 5 :font "courier" :font-size 8)
			    (verbatim
"(defmethod stroke ((box char-box) x y)
  (pdf:in-text-mode
   (pdf:move-text x (+ y (offset box)))
   (pdf:set-font *font* *font-size*)
   (pdf:set-text-x-scale (* *text-x-scale* 100))
   (pdf:show-char (boxed-char box))))"))
		 (paragraph (:h-align :center :font "Times-Italic" :font-size 8 :top-margin 3)
			    "An example of verbatim code.")
		 (paragraph (:h-align :justified :top-margin 9 :font "Helvetica-Oblique"
				      :left-margin 5 :right-margin 5 
				      :font-size 9 :first-line-indent 20)
			   *par1*)
		 (user-drawn-box :dx 240 :dy 100 :stroke-fn 'draw-pie) :eol
		 (paragraph (:h-align :center :font "Times-Italic" :font-size 8)
			    "An example of cl-pdf pie chart inserted.")
		 (paragraph (:h-align :justified :top-margin 9 :font "helvetica" :font-size 9
				      :left-margin 40 :right-margin 40)
			    *par2*)
		 (vspace 10)
		 (paragraph (:h-align :center :top-margin 20 :font "Times-Bold" :font-size 20)
			    "Kerning test" :eol
			    (with-style (:font "Helvetica" :font-size 40 :left-margin 20 :right-margin 20)
			      "Yes, AWAY"))
		 (paragraph (:h-align :center :top-margin 10 :font "CMTI10"
				      :font-size 16 :color '(0 0 0))
			    (with-style (:font "Times-Bold" :font-size 20)
			      "Basic Math Mode Test" :eol)
			    (vspace 5)
			    (display-formula ()
			      (with-style (:font (pdf:get-font "CMEX10" nil) :font-size 30)
				(with-offset (23) "H"))
			     "E"(math-super-and-sub-script () ("n+1") ("k,m"))"="
			     (fraction ()
				       ("x"(with-superscript () "2")"+x-1")
				       ("F(x)+b-3"))
			     "-e"(with-superscript () "-x"(with-superscript () "2")))
			    (vspace 5)
			    (with-style (:font "Times-Roman" :font-size 10)
			      "This test now uses a TeX font (cmti10). Note the italic" :eol "correction for the super/subscript of the E."))
		 (paragraph (:h-align :center :top-margin 20 :font "Times-Italic" :font-size 18 :color '(0.8 0 0))
			      "This test pdf file has been typeset " :eol "with cl-typesetting 0.80" :eol
			      (vspace 10)
			      (with-style (:font "Times-Italic" :font-size 14)
				"Marc Battyani"))
		 :vfill
		 (hrule :dy 20 :stroke-fn 'draw-wavelet-rule :color '(0.8 0 0))
		 :vfill
		 (paragraph (:h-align :center :font "Helvetica-Oblique" :font-size 8)
			    "This project needs contributors. So if you are interested contact "
			    (with-style (:font "Times-Italic" :font-size 9)
			      "marc.battyani@fractalconcept.com") "."
			    ))))
	  (pdf::draw-bar-code128 "CODE128BARCODE" 10 35 :height 25 :width 150 :start-stop-factor 0.25
				 :font-size 7 :show-string t)
	  (draw-block content 40 800 250 380 :rotation 5 :border 0.1)
	  (draw-block content 50 425 250 380 :rotation -5 :border 0.1)
	  (draw-block content 330 800 250 380 :rotation -2  :border 0.1 :special-fn 'link-all-a-and-spaces)
	  (draw-block content 310 400 250 380 :v-align :justified :border 0.1))))
    (pdf:with-page ()
      (pdf:with-outline-level ("cl-typegraph" (pdf:register-page-reference))
	(let ((content
	       (compile-text ()
	          (mark-ref-point "graph")
		  (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 30 :color '(0.0 0 0.8))
			     "cl-typegraph" :eol
			     (vspace 2)
			     (hrule :dy 1)
			     (with-style (:font "Times-Italic" :font-size 13)
			       "The Cool Common Lisp Graph Typesetting System"))
		  (vspace 20)
		  (paragraph (:h-align :justified :top-margin 10 :first-line-indent 10
				       :font "helvetica" :font-size 12)
			     "cl-typegraph is a cl-typesetting extension to typeset graphs. It uses GraphViz for the graph layout and then draws it with cl-pdf and cl-typesetting. The nodes can contain strings or a full cl-typesetting layout." :eol
			     (vspace 20)
			     "In the first graph example below, the nodes contain only strings.")
		  (vspace 20)
		  :hfill (if display-graphs 
                             (graph-box (gen-box-graph))
                             (with-style (:color '(0.0 0 0.8)) "display-graphs is nil")) :hfill
		  (paragraph (:h-align :center :font "Times-Italic" :font-size 11)
			     "The class hierarchy for the boxes in cl-typesetting.")
		  (vspace 20)
		  (paragraph (:h-align :justified :font "helvetica" :font-size 12)
			     "In the next graph, each node contains a full cl-typesetting layout. All the cl-typesetting features can be used in a node, even another graph.")
		  (vspace 20)
		  :hfill (if display-graphs 
                             (graph-box (gen-color-graph))
                             (with-style (:color '(0.0 0 0.8)) "display-graphs is nil")) :hfill
		  (paragraph (:h-align :center :font "Times-Italic" :font-size 11)
			     "The primary colors"))))
	  (draw-block content 30 810 530 780))))
    (pdf:write-document file)))

;;; A higher level example

(defun multi-page-example (&optional (file #P"/tmp/multi-page.pdf")
				     &aux content table (margins '(72 72 72 50)))
  (with-document ()
    ;(pdf:set-line-width 0.1)
   (let* ((print-stamp (multiple-value-bind (second minute hour date month year)
                           (get-decoded-time)
                         (format nil "Printed on ~4D-~2,'0D-~2,'0D ~2,'0D:~2,'0D"
                                 year month date hour minute)))
          (header (compile-text ()
                    (paragraph (:h-align :center
                                         :font "Helvetica-BoldOblique" :font-size 12)
                      "Multi-page example document")
                    ;(vspace 1) ;:vfill
                    (hrule :dy 1/2)))
          (footer (lambda (pdf:*page*)
                    (compile-text (:font "Helvetica" :font-size 10)
                      (hrule :dy 1/2)
                      (hbox (:align :center :adjustable-p t)
                        (verbatim print-stamp)
                        ;(hspace 100)
                        :hfill
                        (verbatim
                         (format nil "Page ~d" pdf:*page-number*)))
                 ))))
    (setq content
          (compile-text ()
            (paragraph (:font "Helvetica-Bold" :font-size 16 :top-margin 20)
              "1. First paragraph group")
            (hrule :dy 2)
            (dotimes (i 40)
              (paragraph (:font "Helvetica" :font-size (+ 6 (random 10)))
                (verbatim (format nil "1.~d. " (1+ i)))
                (dotimes (j (1+ (random 5)))
                  (put-string "The quick brown fox jumps over the lazy dog. "))))
            :eop))
    (draw-pages content :margins margins :header header :footer footer)

    (setq content
          (compile-text ()
            ;(vbox ()
              (paragraph (:font "Times-Bold" :font-size 16 :top-margin 20)
                "2. Second paragraph group"
              (hrule :dy 2))
            (dotimes (i 40)
              (paragraph (:font "Times-Roman" :font-size (+ 6 (random 10)))
                (verbatim (format nil "2.~d. " (1+ i)))
                (dotimes (j (1+ (random 5)))
                  (put-string "The quick brown fox jumps over the lazy dog. "))))
            
            (table (:col-widths '(20 100 100 100) :splittable-p nil
                            :border 0 :background-color '(1 1 0.8))
              (row ()
                (cell (:col-span 4)
                  (paragraph (:h-align :center
                                      :font "Times-Italic" :font-size 12)
                    "Non-splittable table - row with a col-span of 4")))
              (loop for (name tel age) = (list "Dmitri Dmitriev" "555-1234" (+ 25 (random 25)))
                    and row-number from 1 upto 4
                    do
                    (row (:height (when (evenp row-number) 20)
                                  :background-color (if (zerop (mod row-number 3))
                                                        :red nil))
                      (cell () (verbatim
                                        (format nil "~d" row-number)))
                      (cell (:background-color (when (>= age 40) :blue))
                        name)
                      (cell () tel)
                      (cell () (paragraph (:h-align :right) age)))))
          ))
    (draw-pages content :margins margins :header header :footer footer)

    (setq content
          (compile-text ()
          (table (:col-widths '(20 100 100 100) :splittable-p t
                          :border 1/2 :background-color '(1 1 0.8))
            (header-row (:background-color :gray)
              (cell (:row-span 2) (verbatim "Row #"))
              (cell (:row-span 2) "Name")
              (cell () "Telephone")
                ;(table (:col-widths '(100) :splittable-p nil
                ;                :padding 0 :border 1/2); :background-color '(1 1 0.8))
                ;  (row () (cell () "Telephone"))
                ;  (row () (cell () "Fax"))))
              (cell (:row-span 2) (paragraph (:h-align :right) "Age")))
            (header-row (:background-color :gray)
               (cell () "Fax"))
            (footer-row (:background-color :gray)
              (cell (:col-span 4)
                (paragraph (:h-align :center
                                    :font "Times-Italic" :font-size 12)
                  "Table footer with a col-span of 4")))
            (loop for (name tel age) = (list "Ivan Ivanov" "555-1234" (+ 25 (random 25)))
                  and row-number from 1 upto 40
                  do
                  (row (:height (when (evenp row-number) 20)
                                :background-color (if (zerop (mod row-number 3))
                                                      :red nil))
                    (cell () (verbatim
                                      (format nil "~d" row-number)))
                    (cell (:background-color (when (>= age 40) :blue))
                                   name)
                    (cell () tel)
                    (cell () (paragraph (:h-align :right) age)))))))
    (setq table content)
    (draw-pages content :margins margins :header header :footer footer) ;:break :after
    
    (setq content	;; Various spans
          (compile-text ()
          (table (:col-widths '(20 40 60 80 120)
                          :background-color :yellow :border 1
                          :splittable-p t)
            (header-row ()
              (cell (:col-span 5)
		    (paragraph (:h-align :center :font "Times-Italic" :font-size 12)
                  "Table with cells spanning more then one row")))
            (row (:background-color :green)
              (cell (:row-span 2 :background-color :blue)
                "1,1 2,1  row-span 2")
              (cell () "1,2")
              (cell (:col-span 2 :row-span 3 :background-color :red)
                "1,3 1,4 - 3,3 3,4  col-span 2 row-span 3")
              (cell () "1,5"))
            (row ()
              (cell () "2,2")
              (cell (:row-span 2 :background-color :blue) "2,5 3,5  row-span 2"))
            (row (:background-color :green)
              (cell (:col-span 2) "3,1 3,2  col-span 2"))
            (row ()
              (cell () "4,1")
              (cell () "4,2")
              (cell () "4,3")
              (cell () "4,4")
              (cell () "4,5")))))
    (draw-pages content :margins margins :header header :footer footer) ;:break :after

    (draw-block (compile-text () "Test block - line1" :eol "Line 2")
                                 300 300 150 150 :border 1))
   (when pdf:*page* (finalize-page pdf:*page*))
    ;(pdf:with-page ()
     ;(draw-page content :margins 72))
   (pdf:write-document file))
  table)

#+nil
(defun multi-page-hello (&optional (file #P"/tmp/hello.pdf"))
   (pdf:with-document ()
     (let ((content
   (compile-text ()
     (vspace 100)
     (table (:col-widths '(100 200) :splittable-p t)  ;;; start Erik changes
	    (header-row ()
			(cell ()
                         (paragraph () "Header")))
	    (footer-row ()
			(cell ()
                         (paragraph () "Footer")))
            (dotimes (time 50)
              (row ()
                   (cell ()
                         (paragraph () (put-string (format nil "test ~d" time)))))))  ;;; end Erik changes
     (vspace 10)
     :eol 
     (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 50 
                          :color '(0.0 0 0.8))
                "cl-typesetting" :eol
                (vspace 2)
                (hrule :dy 1)
                (with-style (:font "Times-Italic" :font-size 26)
                  "The cool Common Lisp typesetting system")
                (vspace 50)
                (with-style (:font "Times-Italic" :font-size 36 :color '(0.0 0 
                                                                         0.8))
                  (dotimes (i 100)
                    (put-string "Hello World!")(new-line)))))))
       (loop while (boxes content) do
             (pdf:with-page ()
               (pdf:set-line-width 0.1)
               (draw-block content 20 800 545 700))))
     (pdf:write-document file)))

;;; Unicode test

(defparameter *unicode-test-string*
  (map unicode-string-type 'code-char
    '(8252 8319 8359 8592 8593 8594 8595 8596 8597 8616 915 920 934 945 948 949 963 964 966 32
      9554 9555 9556 9557 9558 9559 9560 9561 9562 9563 9564 9565 9566 9567 32 65
      9568 9650 9658 9660 9668 9675 9688 9689 8364 1027 8218 402 8222 8230 8224 8225 32 66
      372 373 374 375 383 506 507 508 509 510 511 903 913 914 916 917 918 919 921 922 923 32
      946 947 950 951 952 953 954 955 956 957 958 1101 1102 1103 1105 1106 1107 1108 32
      1475 1488 1489 1490 1491 1492 1493 64304 64305 64306 64307 64308 64309 32
      7911 7912 7913 7914 7915 7916 7917 7918 1179 1180 1181 1186 1187 1198 1199 1200 32)))

;; Look at the unicode-readme.txt in cl-pdf to see how to load unicode fonts
(defun unicode-hello (&optional (file #P"/tmp/hello-u.pdf"))
  (pdf:with-document ()
    (pdf:with-page ()
      (pdf:with-outline-level ("Unicode Example" (pdf:register-page-reference))
	(pdf:set-line-width 0.1)
	(let ((content
	       (compile-text ()
			     (vspace 100)
		 (paragraph (:h-align :center :font "Helvetica-Bold" :font-size 50 :color '(0.0 0 0.8))
			    "cl-typesetting" :eol
			    (vspace 2)
			    (hrule :dy 1)
			    (with-style (:font "Times-Italic" :font-size 26)
			      "The cool Common Lisp typesetting system")
			    (vspace 50)
			    (with-style (:font "TimesNewRomanPSMT" :font-size 36)
			      (put-string *unicode-test-string*))))))
	  (draw-block content 20 800 545 700))))
    (pdf:write-document file)))
