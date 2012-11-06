
(progn ;; FIXME: read data from CSV file.  To do.

  
  ;; challenge is to ensure that we get mixed arrays when we want them,
  ;; and single-type (simple) arrays in other cases.


  (defparameter *csv-num*
    (cybertiggyr-dsv::load-escaped
     #p"/media/disk/Desktop/sandbox/CLS.git/Data/example-numeric.csv"
     :field-separator #\,
     :trace T))

  (nth 0 (nth 0 *csv-num*))

  (defparameter *csv-num*
    (cybertiggyr-dsv::load-escaped
     #p"/media/disk/Desktop/sandbox/CLS.git/Data/example-numeric2.dsv"
     :field-separator #\:))

  (nth 0 (nth 0 *csv-num*))


  ;; The handling of these types should be compariable to what we do for
  ;; matrices, but without the numerical processing.  i.e. mref, bind2,
  ;; make-dataframe, and the class structure should be similar. 
  
  ;; With numerical data, there should be a straightforward mapping from
  ;; the data.frame to a matrix.   With categorical data (including
  ;; dense categories such as doc-strings, as well as sparse categories
  ;; such as binary data), we need to include metadata about ordering,
  ;; coding, and such.  So the structures should probably consider 

  ;; Using the CSV file:

  (defun parse-number (s)
    (let* ((*read-eval* nil)
	   (n (read-from-string s)))
      (if (numberp n) n)))

  (parse-number "34")
  (parse-number "34 ")
  (parse-number " 34")
  (parse-number " 34 ")

  (+  (parse-number "3.4") 3)
  (parse-number "3.4 ")
  (parse-number " 3.4")
  (+  (parse-number " 3.4 ") 3)

  (parse-number "a")

  ;; (coerce "2.3" 'number)  => ERROR
  ;; (coerce "2" 'float)  => ERROR
  

  ;; cases, simple to not so
  (defparameter *test-string1* "1.2")
  (defparameter *test-string2* " 1.2")
  (defparameter *test-string3* " 1.2 ")
  )

