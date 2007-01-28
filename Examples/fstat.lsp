(defun f-statistic (m1 m2)
"
Args: (m1 m2)
Computes the F statistic for testing model m1 within model m2."
  (let ((ss1 (send m1 :sum-of-squares))
        (df1 (send m1 :df))
        (ss2 (send m2 :sum-of-squares))
        (df2 (send m2 :df)))
    (/ (/ (- ss1 ss2) (- df1 df2)) (/ ss2 df2))))

