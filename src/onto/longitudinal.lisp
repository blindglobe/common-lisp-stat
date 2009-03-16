
(in-package :data-analysis-ontology)

(session :conference "ENAR" :session "longitudinal data" :year 2009

 (talk :name "Jianwen Cai"
       :title "Joint Modeling of Longituindal Cat data and Surv data"
       :topic "joint survival longitudinal"

       (example "liver transplant"
		:goal " baseline-chars disease-hist impact QOL and risk of death postliver transpliatatn"

		(data 	:time 4m 1y 2y 3y 4y 5y  
			:measure QOL (binary QOL ) (time death)))

       (existing methods
		 (selection "Tsiatis, T+ Wulfsohn, etc"
			    estimate (conditional  (survival-time T)
						   (longitudinal data X)))
		 (pattern mixture models )
		 (simultaneous model "Zheng Cai 2005"
			       (assume: (longitudinal-outcomes 'normal)))
		 ())

       (proposal  
	(model	(GLMM longitudinal outcome, random on subject)
		(PH   T conditionl on random effects above and obsd history before t))

	(data
	 (Ni Yij Xij XTij)
	 (Zi Di Wi (t) WTi (t)))
	
	(inference  (parameters RE FE baseline CumHaz)
		    (MLE 'complete-likelihood  'marginal on RE)
		    
		    (compute-with EM 'TLouis-est-var))

	(behavior (asympt RE FE CumHaz :consistent (product-norm (theta euclidean-distance) 
								 (cumhaz sup-norm)))
		  (finite-sample :assume  (list  (longitudinal '(regression 'logistic))
						 (event-time 'PH-model)) 
				 :results (fine under friendly conditions)) ; FUFC
		  )
	
	(example "Liver Transplant"
		 :N 582
		 :events 76
		 :QOL-obsd 1382 )

	(summary :weird normal distr assumptions for REs ?)))

 (talk :name "Mike Kenward"
       :title "Dropout in Long clin trial w/ binary outcome"
       :topic "clin trial; longitudinal ; binary response ; dropout/withdrawal"
       :goal "compare at endtime, endpoints"

       (prior-art "contin outomces with MAR, MVN framework")
       (issue "no natural generalization for MV binary resp")
       (issue (contrast 'population-average 'subject-specific)) ; marginal conditional

       (with-missing-data (assert 'likelihood works) (assert 'estimating-equation issues))
       
       (priot-art
	(list  ( Fitzmaurice 'cluster-weighted-gee)
	       ( Robins/Rotnitsky 'obn-weighted-gee)
	       ( Paik 'MI-GEE :multiple-imputation followed by 'complete-data-analysis
		      (meng 'imputation model conditions))
	       ( Augmented Inverse Prob Wgt Est :approach " add-term with zero-expectation to incr eff, 
			   see also scharfstein" :name AIPW)
	        ( doubly robust multiple imputation , but restricted
	to single var dropout, but 
			extended/made practical by Bang and Robins,
	:limit (assert monotone) (var-est by bootstrap ) (models for
	awkward expectations) ) 

	       )
	(Proposal 
	 :name "doubly robust MI" 
	 :prop "asympt equiv bang/robins"
	 :alg (steps  MI over seq of regressions, Rubin's MI var est possible
	      (not just bootstrap))
	 :theory ( (obs-weight-gee Aug-obs-weight-gee d ) asympt equiv	for disc data)

	 )

	(conclusion
	 (MI-GEE more eff than obs-wgt-GEE, up to N=5000)
	 (aux term incr eff/rob)
	 (DRMI (sllight less eff than MI-GEE with Y corr spec)
	       (simple var est)
	       (practical implem use (simple / clear)))))  )

(talk :name "Xihong Lin"
      :title "variable selection using dantig selector"
      :topic ""
      :goal ""

      (prior-art "Candes and Tao 2007" Dantzig Selector
		 (resemebles 'estimating-equation based var-select procedure))

      (proposal "extend to corr data")
      (goal "select coefs which are non-zero")

      (prior-art :penalized-likelihood)
      (prior-art :regularization)

      (control correlation-of-residuals, not size of residuals!)

      (subject-to (min (norm 1 beta))
		  (norm inf (m* Xt (Y - Xb))))

      (note (there-exists (class matrices) st danztig-selector
      inconsistent for model selection))
      
      (solution :weight selector through penalization to remove
      "droppings")
      
      ;;; 
      ))

(session "missing data"

	 (talk :name "Geert M"
	       :title "MNAR equiv to so MAR"

	       (example toe nail data, longitudinal measure of serer toenail inferctions)
	       (example slovenian public opiion survey on independence
			3 q's : seccession-ind attend indep-grp ?)
	       
	       (frameworks (list selection
				 pattern-mixture
				 shared-parameter)
			   :missing-models MCAR->MAR->MNAR)

	       (assumption "not dependence on future")
	       (approach "put past responses into future not, AR/markov style")
	       (everything MNAR model has MAR counterpart)

	       (explicit guide to construction)

	       (example :toe-nail infection severity within as a
	       past/current marker to be managed when missing) )


	 
	 )


