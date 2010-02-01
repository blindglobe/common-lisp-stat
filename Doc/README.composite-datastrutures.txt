
* Overview

  Composite datastructures are a key component of CLS.  We inherit
  from a number of packages to maintain both the ability to generalize
  as well as to leverage others work for optimization.

* dependency structure, package-wise

** lift

** xarray

** cffi 

** cl-utilities

** FNV

** CL-BLAPACK : CFFI

** FFA

** listoflist
   xarray
   lift

** lisp-matrix
   xarray, lift, listoflist, cffi, cl-utilities, FNV,
              cl-blapack, FFA


** CLS 
   xarray   (for DFs)
   lift
   


