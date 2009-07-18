
CL based design needs to consider the packaging components, to ensure
that the final packaging works.

Some guidelines:  packages should have methods which self-describe
exported commands. 

We can do this via lisp functions -- need a macro to provide
reflection of this information.

can we load packages and add symbols to an existing package --
i.e. want to be able to load into curent package or another package as
specified, i.e. 
(load-lisp-stat-package package-to-load
                        package-space-to-infect)

(default is ls-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;; SOME CL EXAMPLES and GUIDANCE.

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



#|
 (defun testme (&key (a 3) (b (+ a 3)))
   b)
 
 (testme)
 (testme :a 2)
 (testme :b 4)
 (testme :a 2 :b (* a 5))
|#


#|
 (defun testme (&key (a 3) (b (+ a 3)))
   b)
 
 (testme)
 (testme :a 2)
 (testme :b 4)
 (testme :a 2 :b (* a 5))
|#

