
(require 'asdf)

(asdf:operate 'asdf:load-op 'clem)
(asdf:operate 'asdf:load-op 'clem-test)

(let ((m1 (make-instance 'clem:ub8-matrix :rows 8 :cols 10 :initial-element 2))
      (m2 (make-instance 'clem:ub8-matrix :rows 10 :cols 7 :initial-element 3)))
  (describe m1)
  (describe (clem::horzcat m1 m2))
  (describe (clem::vertcat m1 m2)))

;(clem-test:run-tests)
;(clem-test:run-bench)
; (clem-test:run-defmatrix-tests)
; (let* ((x (clem::random-fixnum-matrix 512 512 :max 255))) (print (clem::sum-range x 2 2 4 4)))


