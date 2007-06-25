		  Common-Lisp implementations of the
		  ==================================

		   grid-restrained and traditional
		   ===============================

			Nelder-Mead algorithms
			======================



Introduction
------------

  These common lisp sources contain two variants of the Nelder-Mead
  algorithm. The original algorithm [1] and a provably convergent,
  reliable variant by A. Bürmen et al [4], called the "Grid Restrained
  Nelder Mead Algorithm" (GRNMA).

  It should be mentioned that other, provably convergent variant exist
  [2,3], which aren't included here. The only reasons are lack of
  time, and the fact that the implemented variant does not require a
  simple descent condition, putting it closer to the original.

  Other than that, and based on the article [4], the performance of
  these methods seems to be about equal in terms of number of function
  evaluations. As a side effect of the additional reliability, both
  tend to be a lot more efficient than the original algorithm even
  when it does not fail. In particular when the number of dimensions
  increases. As a test, one might try the GRNM,

  (grnm-optimize #'standard-quadratic
                  (make-array 30 :initial-element 1.0d0) :verbose t)

  and compare with the original Nelder-Mead,

  (nm-optimize #'standard-quadratic
                (make-array 30 :initial-element 1.0d0) :verbose t)

  and observe the difference in number of function evaluations (the
  last value returned).

  (This exercise also serves to illustrate the overall deficiencies of
  direct search algorithms when applied to higher dimensional
  problems.)

  This software is provided under the MIT licence; see COPYING.txt for
  details.

Usage
-----

  This implementation of the grid restrained Nelder-Mead algorithm
  expects at least two parameters: the objective function, and an
  initial guess. It returns four values;

   * the minimizer,
   * the minimum,
   * the last simplex,
   * and the number of function evaluations.

  The objective function should accept a double-float array as its
  argument.

  The initial guess will usually be an array of double-float numbers
  (but instead an object of the class NM-SIMPLEX may also be provided;
  see below). For example,

    (grnm-optimize #'rosenbrock #(40.0d0 40.0d0))

  finds the minimum of the Rosenbrock function, and

    (grnm-optimize #'standard-quadratic
                   (make-array 30 :initial-element 1.0d0))

  finds the minimum of the standard quadratic function in 30
  dimensions.

  A few keyword arguments can customize the behavior. These are

    :verbose  (default: NIL)
    :converged-p (default: (burmen-et-al-convergence-test))
    :max-function-calls (default: NIL; => as many as needed)


  :verbose (default: NIL)

    Pass T here if you want to see some progress report. The amount of
    output can be controlled by setting *verbose-level* to 1 or 2. The
    difference is that with 1 (the default) only the best value of the
    simplex is shown, while with 2 the whole simplex is printed on
    each iteration.

  :converged-p (default: (burmen-et-al-convergence-test))

    The burmen-et-al-convergence-test is, as the name suggests, the
    convergence test used in the article by Burmen et al. It accepts a
    few parameters: tol-x, tol-f and rel; please see the article for
    further details.

    Another convergence criterion that can be given is (pp-volume-test
    <tol>), which returns true once the parallelogram(!) spanned by the
    vertices of the simplex has a volume lower than <tol> to the power
    of N, where N is the dimension of the problem. This is a rather
    expensive test, as it involves computing a QR decomposition of an N
    by N matrix on each call.

    If you are not in the mood of taking prisoners, you might as well
    pass (constantly NIL) as the convergence criterion. This has as a
    consequence that the iteration continues until the simplex
    collapses, which in floating-point arithmetic happens in finite
    time. The grid restrained Nelder-Mead algorithm should have
    converged by then.

  :max-function-calls (default: NIL)

    Maximum number of objective function evaluations. After that many
    function evaluations, this implementation of the algorithm will
    declare convergence to have occurred.

    The actual number of function calls might be slightly larger (at
    most by N), as the relevant condition is only checked in certain
    situations.

Utilities/Misc
--------------

  NM-optimize

    Apart from the grid-restrained Nelder Mead algorithm, the
    traditional variant is also provided. The corresponding function
    is named NM-optimize, and its usage is the same as for
    GRNM-optimize.

    The only difference is that :max-function-calls has a default
    value of 100000. Otherwise the algorithm might well iterate
    forever.

  Initial-simplex <initial guess> :displace <displacement>

    Constructs an initial simplex with the double-float array <initial
    guess> as on of its corners.

    The displacement can be a an array of double floats or a
    number. In the first case, the additional vertices of the simplex
    are build by adding to each component of the <initial guess> the
    corresponding component in <displacement>. If it is a number, then
    the additional vertices are build by adding <displacement> to each
    component of the <initial guess>.

Tips & Tricks
-------------

  * One can try to solve constrained optimization problems by returning

      MOST-POSITIVE-DOUBLE-FLOAT

    whenever the objective function is called with an argument that
    violates the constraints. Mathematically, this falls out of the
    theory, but it works most of the time.

  * If your objective function is noisy or not smooth (for instance, if
    its first derivatives are not continuous) it is a good idea to
    restart the algorithm. Remember that convergence is only
    guaranteed if the objective function is at least C^1.

References
----------

[1] J.A. Nelder and R. Mead, "A simplex method for function
    minimization," The Computer Journal, vol. 7, pp. 308-313, 1965.


[2] P. Tseng, "Fortified-descent simplicial search method: A general
    approach," SIAM Journal on Optimization, vol. 10, pp. 269-288,
    1999.

[3] C.J. Price, I.D. Coope, and D. Byatt, "A convergent variant of the
    Nelder-Mead algorithm," Journal of Optimization Theory and
    Applications, vol. 113, pp. 5-19, 2002.

[4] A. Bürmen, J. Puhan and T. Tuma, "Grid Restrained Nelder-Mead
    Algorithm", Computational Optimization and Applications, vol.
    34, no. 3, pp. 359 - 375, 2006.


--------
Mario S. Mommer, November 2006
