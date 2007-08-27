(#.
 (cons
  :p
  (progn
    (in-package :clem)
    (defparameter smarkup::*document-thesis* nil)
    (smarkup::setup-headings)
    nil))
 
 (:smarkup-metadata
  (:copyright
   "Copyright 2006, Cyrus Harmon. All Rights Reserved.")
  (:title "clem: A common-lisp matrix package")
  (:author "Cyrus L. Harmon")
  (:bibtex-database
   "(\"asdf:/ch-bib/lisp\" \"asdf:/ch-bib/bio\")")
  (:bibtex-style "Science"))
 (:html-metadata (:htmlcss "simple.css") )
 
 (:lisp-silent 
  "(asdf:operate 'asdf:load-op 'clem)"
  "(setf smarkup::*baseline-skip* \"14pt\")"
  "(setf smarkup::*par-skip* \"0pt\")")
 
 (:span
  (:h1 "Abstract")
 
  (:p "CLEM is an open-source Common Lisp library for the
 representation and manipulation of matrices. CLEM is designed to
 be a flexible and extensible system for the representation of
 arbitrary 2-dimensional matrices."))
 
 (:h1 "Introduction")

 (:p "The Common Lisp language"
     (:bibcite "steele1990common")
     " offers a rich, dynamic environment for programming and
data analysis. Common Lisp contains a powerful object system, the
Common Lisp Object System (CLOS)"
     (:bibcite "keene1989object")
     ", and most modern implementations support a protocol for
the generation not just of new classes and objects, but to extend
the object system itself using the Meta-object Protocol"
     (:bibcite "kiczales1991art")
     ".")

 (:p "CLEM uses CLOS and the Meta-object protocol (MOP) to define a"
     (:code "standard-matrix-class")
     " that serves as the metaclass for classes that represent
matrices with elements of specific types. The typed matrices can
represent matrices containing values of specific types in the
Common Lisp type system, starting with type "
     (:code "t")
     " as the most general data type, and becoming more restrictive by using more specific types such"
     (:code "double-float")
     ", "
     (:code "fixnum")
     ", or "
     (:code "(unsigned-byte 8)")
     ". By using the most specific type that can represent the values of a given matrix, the lisp system can optimize for better performance and memory usage requirements. For example, a "
     (:code "bit-matrix")
     " will use 1 bit per matrix element, rather than 32-bits on 32-bit systems for a "
     (:code "t-matrix")
     ".")

 (:h2 "Matrix Types")

 (:h2 "Matrix Representation")

 (:p "Common Lisp provides a rich built-in array type which serves as
 the storage for CLEM matrices. Given that Common Lisp has built-in
 arrays, why do we need CLEM and what value is provided by creating a
 set of classses around arrays? First, the Common Lisp arrays have a
 limited set of operations defined on them. While there is a built-in
 (scalar) addition operator, there is no built-in way to perform an
 element-wise addition of two arrays. CLEM addresses these by defining
 a set of generic functions that operate on matrices that provide a
 number of commonly used matrix operations such as matrix
 arithmetic. Second, there is no way to define methods on arrays based
 on their element types. Therefore, we define subclasses of matrix
 whose underlying arrays are specialized to distinct types. We can
 then define methods to operate specifically on these subclasses,
 affording the opportunity to treat, say, floating point and integer
 matrices differently and to provide declarations to the compiler
 based on the array element type, which can, in Common Lisp
 implementations with sufficiently smart compilers, lead to much
 improved performance.")
 
 (:h1 "Defining CLEM Classes and Making CLEM Instances")

 (:h2 "Creating CLEM Instances with make-instance")

 (:p "The following code creates a 16-row by 16-column matrix of type"
     (:code "double-float-matrix")
     " and assigns it to the dynamic variable"
     (:code "*m1*")
     ".")
 (:lisp 
  "(defparameter *m1*
 (make-instance 'clem:double-float-matrix :rows 16 :cols 16))"
  "*m1*")

 (:p "The default is to only show the first 7 and the last rows
 and columns of each matrix. The number of rows and columns can
 be changed by setting the "
     (:code "*matrix-print-row-limit*")
     " and"
     (:code "*matrix-print-col-limit*")
     " variables.")

 (:h2 "standard-matrix-class")
 (:h2 "CLEM Matrix Types")

 (:h3 "Number matrices")

 (:p "The most general class of numerical matrix is the number matrix.")

 (:h3 "Integer Matrices")

 (:h3 "Floating-point Matrices")

 (:h3 "Complex-value Matrices")

 (:h1 "Working with CLEM Matrices")

 (:h2 "Matrix Dimensions and Values")

 (:h2 "Typed matrix operations")

 (:h2 "Matrix Copying")

 (:h2 "matrix-move")

 (:h1 "Matrix Arithmetic")

 (:h2 "Matrix Addition and Subtraction")

 (:h2 "Matrix Multiplication")

 (:h2 "Hadamard Product")

 (:h2 "Scalar Arithmetic")

 (:h2 "Other Mathematical Functions")

 (:p "Discuss mat-log, mat-abs, min, and max.")

 (:h1 "Matrix Operations")

 (:h2 "Matrix Inversion")

 (:h2 "Matrix Normalization")

 (:h2 "Discrete Convolution")

 (:h3 "Derivatives")

 (:h3 "Gradient Magnitude")

 (:h3 "Gaussian Blur")

 (:h2 "Affine Transformations")

 (:h3 "Interpolation")

 (:h2 "Morphological Operations")

 (:h3 "Dilation and Erosion")

 (:h3 "Variance")

 (:h3 "Thresholding")

 (:h1 "CLEM Implementation Details")

 (:h2 "Type-specific matrix functions")

 (:p "The general strategy has been to 1) make things work and
 then make them work quickly.  To this end, I have been writing
 functions for matrix operations in a general manner first and
 then recoding type-specific versions to make certain operations
 go faster.  This is done via liberal use of macros to generate
 type-specific functions and methods for matrix operations that
 go much faster than the general versions.")

 (:p "The convention is that a generic function such as sum-range
 will have a generic version that works with all matrices and
 type specific versions thaqt work with specific matrices. g In
 order to support these functions there may be internal methods,
 prefixed with a %, that implement certain type-specific
 functionality.  Macros that generate the code used for the
 type-specific methods will be prefixed with a %%.  In theory,
 the %%-macros can be called from other code that need to
 generate in-place code where the overhead of the method-call to
 the %-method would be too expensive. This convention is not yet
 widely enforced and certainly untested. Hopefully this situation
 will improve.")

 (:h2 "Hacking the SBCL compiler to improve performance")

 (:bibliography))
