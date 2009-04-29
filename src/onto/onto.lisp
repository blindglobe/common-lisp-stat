

#+nil
(progn ;; philosophy time
  
  (setf my-model (model :name "ex1"
			:data-slots (list w x y z)
			:param-slots (list alpha beta gamma)
			:math-form (regression-model :formula '(= w (+ (* beta x)
								     (* alpha y)
								     (* gamma z)
								     normal-error))
						     :centrality 'median ; 'mean
						     )))

#| or:
  #R"W ~ x+  y + z "			
|#

  (setf my-dataset (statistical-table :table data-frame-contents
				      :metadata (list (:case-names (list ))
						      (:var-names (list ))
						      (:documentation "string of doc"))))

  (setf my-analysis (analysis
		     :model my-model
		     :data my-dataset
		     :parameter-map (pairing (model-param-slots my-model)
					     (data-var-names my-dataset))))

  ;; ontological implications -- the analysis is an abstract class of
  ;; data, model, and mapping between the model and data.  The fit is
  ;; the instantiation of such.  This provides a statistical object
  ;; computation theory which can be realized as "executable
  ;; statistics" or "computable statistics".
  (setf my-analysis (analyze my-fit
			     :estimation-method 'linear-least-squares-regression))

  ;; one of the tricks here is that one needs to provide the structure
  ;; from which to consider estimation, and more importantly, the
  ;; validity of the estimation.

  ;;
  (setf linear-least-squares-regression
	(estimation-method-definition
	 :variable-defintions ((list 
				;; from MachLearn: supervised,
				;; unsupervised
				:data-response-vars list-drv ; nil if unsup
				;; 
				:param-vars list-pv
				     :data-predictor-vars list-dpv 
				     ;; nil in this case.  these
				     ;; describe "out-of-box" specs
				     :hyper-vars list-hv))
	 :form '(regression-additive-error
		 :central-form (linear-form drv pv dpv)
		 :error-form 'normal-error)
	 :resulting-decision '(point-estimation interval-estimation)
	 :philosophy 'frequentist
	 :documentation "use least squares to fit a linear regression
                         model to data."))

  (defparameter *statistical-philosophies*
    '(frequentist bayesian fiducial decision-analysis)
    "can be combined to build decision-making approaches and
    characterizations")

  (defparameter *decisions*
    '(estimation selection testing)
    "possible results from a...")
  ;; is this really true?  One can embedded hypothesis testing within
  ;; estimation, as the hypothesis estimated to select.  And
  ;; categorical/continuous rear their ugly heads, but not really in
  ;; an essential way.

  (defparameter *ontology-of-decision-procedures*
    (list :decisions
	  (list :estimation
		(list :point
		      (list :maximum-likelihood
			    :minimum-entropy
			    :least-squares
			    :method-of-moments)
		      :interval
		      (list :maximum-likelihood
			    :))
		:testing
		(list :fisherian
		      :neyman-pearson
		      (list :traditional
			    :bioequivalence-inversion)
		      :selection
		      (list :ranking
			    :top-k-of-n-select))
		:parametric
		:partially-parametric))
    "start of ontology"))
