
(in-package :cls-dataframe)

(defgeneric nvars (df)
  (:documentation  "number of variables represented in storage type.")
  (:method ( (df matrix-like))
    (ncols df))
  (:method ( (df dataframe-array))
    (ncols df))
  (:method ((df list))
    (ncols list))
  (:method ((df array))
    (array-dimension  df 1)))


(defgeneric ncases (df)
  (:documentation "number of cases (indep, or indep within context,
  observantions) within DF storage form.")
  (:method ((df simple-array))
    (array-dimension df 0))
  (:method ((df matrix-like))
    (nrows df))
  (:method ((df list))
    (nrows df)) ;; probably should do a valid LISTOFLIST structure
		;; test but this would be inefficient
  (:method ((df dataframe-array))
    (nrows df))
  (:method ((df array))
    (array-dimension df 0)))

(defgeneric dfcolumn (df  variable)
  (:documentation "generic column getter")
  (:method ( (df dataframe-like) variable)
    (loop for the-row below (ncases df) collect (xref df the-row variable))))

(defgeneric dfrow (df row)
  (:documentation "generic row getter")
  (:method (( df dataframe-like) row)
    (loop for column below (nvars df) collect (xref df row column) )))

(defun  df->grid (df &rest cols)
   "A helper function that creates a foreign grid of (ncase df) and (length cols) specifically for passing to gsll. If a column is a date then it will be converted into the equivalent fixnum representation - thats not implemented as yet. if colls is the single keyword  :all then just do all the cols "
  (flet ((create-subset (df  cols)
	   "return selected columns as a list"
	   (loop for the-row below (ncases df)
		 collect (loop for col in cols
			       collect (xref df the-row))))
	 (translate-columns (&rest cols)
	   "convert list of column keywords to indexes"
	   (if (equal cols :all)
	       (alexandria:iota (nvars df))
	       (loop for c in cols
		     collect (translate-column df c))))))
  (grid:make-foreign-array 'double-float
			   :initial-contents (create-subset df (translate-columns cols)))
  )