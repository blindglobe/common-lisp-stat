
;;; GC/MS : gas chromatograph / mass-soec
;;; data:
;;; time / ion  (2d graph)

(talk :name "philip dixon"
;; distance measures

;; spectral fingerprints
;; peak fingerprints
;; concentrations
;; concentrations w/ ID'd compounds

(def data (2d :x time :y signal) )


( Distances:
  PCA of euclidean distance matrix
  (70+ diff distance measures)

  (traditional-chocie: euclidane distance)
  (decision (or  tradition  arbitrary))  )

(distance measures
  (usually location-invariant)
  (this might not be true! (2->3 not same as 200->201))

  (weighted distance measures, 
     (incorporation mean-variance relationship)
     )
  (defun canberra-dsitance (x y) (sum (/ (abs (- y z))
					 (power (- y z) 2))))

  (:property post-weight, distance unchanges by metabolite-spec scaling)

  (choose distance-measure

      (important? choice)	 
      (good-chars (consistent across choice of metabolite)
		  ())

      weighting seems to be more impactful than selectino of distance
      
      (robustness across metabolite within distance measure)
	  ))

(extensioon below-limit-of-detection, usually fixed imputation (more a LOQ than LOD case)
	    (manage by increasing variation of measurement, in essence down-weighting))

(summary var-mean relationship helps selection of distance measure
 plus: incorporates lower precision of below-LOQ values)

)

(talk :presenter "Heike Hofmann"
      :title "itneractive grahics  with metabolomic data preprocessing"
      

      (data preproc goals
	    (extract peaks in GC-MS)
	    (quant amount in the peaks)
	    (grou in metabolite profesil)
	    (ID metaboites))

      (realized
        (time vs (/ m z) ))

      (standard sensible selection using assumption of baseline overall noise)

      (sumamrize AUC for peak, sum for a metabolite   )
      (normalize across metabolutes)

      (metabolite lookup:
		  NIST library ($4k)
		  MeT-RO coming
		  GOLM (not updated since 2004))
      (example 4 bio reps arabidopsis WT and genotype

	       (compare (old preproc Msfacts RT matching, normalized to ISTD)
			(new preproc MetIDEA, log2 transform mean centered)
			(chromatoplots "our method"))))
