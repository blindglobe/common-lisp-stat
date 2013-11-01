;;; -*- mode: lisp -*-

;;; Time-stamp: <2013-11-01 09:05:51 tony> 
;;; Creation:   <2005-08-xx 21:34:07 rossini> 
;;; File:       data.lisp
;;; Author:     AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2005--2009, AJ Rossini.  GPLv2
;;; Purpose:    data package for lispstat

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

;;; This organization and structure is new to the 21st Century
;;; version.

(in-package :cls-data)

;;; The purpose of this package is to manage data which will be
;;; processed by LispStat.  In particular, it will be important to
;;; register variables, datasets, relational structures, and other
;;; objects which could be the target for statistical modeling and
;;; inference.

;;; data management, in the context of this system, needs to consider
;;; the following: 
;;; # multiscale metadata: describing at the variable, observation,
;;;   dataset, and collection-of-datasets scales
;;; # data import: push-based (ETL-functionality (extract, t###,
;;;   load) externally driven); and pull-based (std data import
;;;   functionality) 
;;; # triggers or conditions, for automating situational events
;;; # data export functionality

;;; consider that data has 3 genotypic characteristics.
;;; #1: storage form -- scalar, vector, array.  
;;; #2: datarep ("computer science simplistic data") type, such as
;;;     integer, real, string, symbol.
;;; #3: statrep (statistical type), "usually handled by computer
;;;     science approaches via metadata", augmenting datarep type with
;;;     use in a statistical context, i.e. that would include nominal,
;;;     ordinal, integer, continous, interval (orderable subtypes).
;;;
;;; Clearly, the statistical type can be inherited, likewise the
;;; numerical type as well.  The form can be pushed up or simplified
;;; as necessary, but this can be challenging.

;;; The first approach considered is for CLS to handle this as
;;; lisp-only structures.  When we realize an "abstract" model, the
;;; data should be able to be pushed by appropriate triggers (either
;;; "en masse", or "on-demand") into an appropriate linear algebra
;;; framework.

;;; There is some excellent material on this by John Chambers in one
;;; of his earlier books.  Reference is being ignored to encourage
;;; people to read them all.  With all due respect to John, they've
;;; lasted quite well, but need to be updated.


;;; Data (storage) Types, dt-{.*}
;;;
;;; Data types are the representation of data from a computer-science
;;; perspective, i.e. what it is that they contain, in the sense of
;;; scalars, arrays, networks, but not the actual values or
;;; statistical behavour of the values.  These types include
;;; particular forms of compound types (i.e. dataframe is array-like,
;;; but types differ, difference is row-wise, while array is a
;;; compound of elements of the same type.
;;; 
;;; This is completely subject to change, AND HAS.  We use a class
;;; heirarchy to generate the types, deriving from the virtual
;;; dataframe-like and matrix-like classes to construct what we think
;;; we might need, in terms of variables, observations, datasets, and
;;; collections-of-datasets.

;;; Statistical Variable Types, sv-{.*} or statistical-variable-{.*}
;;; 
;;; Statistical variable types work to represent the statistical
;;; category represented by the variable, i.e. nominal, ordinal,
;;; integral, continous, ratio.   This metadata can be used to hint at
;;; appropriate analysis methods -- or perhaps more critically, to
;;; define how these methods will fail in the final interrpretation.  

;;; originally, these were considered to be types, but now, we
;;; consider this in terms of abstract classes and mix-ins.

;;; STATISTICAL VARIABLES SHOULD BE XARRAY'd

;; Need to distinguish between empirical and model-based realizations
;; of variables.   Do we balance by API design, or should we ensure that
;; one or the other is more critical (via naming convention of adding
;; description to class name)?

;; Current class heirarchy is 
;; statistical-variable         
;; how described: empiricial or model-based
;; modality (categorical -  nominal, ordinal, integral
;;          (continuous -   real, rational, 
;;


(defclass statistical-variable ()
  ((number-of-observations
    :initform 0
    :initarg :nobs
    :accessor nobs
    ;; :type generalized-sequence ; sequence or
    ;; array
    :documentation "virtual class for statistical variables
                    (assuming design, marginalization, and
                    conditioning to create the current dataset from
                    which this variable came from)."))
  (:documentation "virtual class indicating that we are working with a
    statistical variable (arising from a actual set of observation or
    a virtual / hypothesized set)."))

(defclass empirical-variable (statistical-variable)
  ((number-of-observations :initform 0
			   :initarg :nobs
			   :accessor nobs
			   ;; :type generalized-sequence ; sequence or
			   ;; array
			   :documentation "number of statistically
    independent observations in the current context (assuming design,
    marginalization, and conditioning to create the current dataset
    from which this variable came from)."))
  (:documentation "mix-in class that  indicating that we are working with a
    statistical variable which arises from a actual set of observation or
    a virtual / hypothesized set)."))

(defclass modelbased-variable (statistical-variable)
  ((density/mass-function :initform nil
			  :initarg :pdmf
			  :accessor pdmf
			  :type function
			  :documentation "core function indicating
    probability of a set of observations")
   (draw-function :initform nil
		  :initarg :drawf
		  :accessor draw  ; must match cl-random API
		  :type function
		  :documentation "function for drawing an observation,
    should take an optional RV arg for selecting the stream to draw
    from."))
  (:documentation "model-based statistical variables have observations
    which come from a model.  Core information is simply how to
    compute probabilities, and how to draw a new realization.  All
    else should be deriveable from these two.  Possibly we need
    additional metadata for working with these?"))

(defclass categorical-statistical-variable
    (statistical-variable)
  ((factor-levels :initform nil
		  :initarg :factor-levels
		  :accessor factor-levels
		  :type sequence
		  :documentation "the possible levels which the
    variable may take.  These should be a (possibly proper) superset
    of the actual current levels observed in the variable.")))

(defclass nominal-statistical-variable
    (categorical-statistical-variable)
  ()
  (:documentation "currently identical to categorical variable, no
    true difference from the most general state."))

(defclass ordinal-statistical-variable
    (nominal-statistical-variable)
  ((ordering :initform nil
	     :initarg :ordering
	     :accessor ordering
	     :type sequence
	     :documentation "levels are completely ordered, and this
    should be an ordered sequence (prefer array/vector?) of unique
    levels. (do we need a partially ordered variant?)"))
  (:documentation "categorical variable whose levels are completely ordered."))

(defclass continuous-statistical-variable
    (statistical-variable)
  ((support :initform nil
	    :accessor support
	    :type sequence
	    :documentation "Support is used in the sense of
    probability support, and should be a range, list of ranges, t
    (indicating whole space), or nil (indicating measure-0 space)."))
  (:documentation "empirical characteristics for a continuous
    statistical variable"))

(defmethod print-object ((object statistical-variable) stream)
  "Need to work through how to print various objects.  Statvars don't
necessarily have data yet!"
  (print-unreadable-object (object stream :type t)
    (format stream "nobs=~d" (nobs object))))

(defmethod print-object ((object categorical-statistical-variable) stream)
  "Need to work through how to print various objects.  Statvars don't
necessarily have data yet!  Here, we should print out the stat-var
information, (pass to superclass) and then print out factor levels if
short enough (exact class).  Useful to review methods-mixing for
this, first bit should be indentical to stat-var."
  (print-unreadable-object (object stream :type t)
    (format stream "nobs=~d" (nobs object))
    (format stream "levels=~A" (factor-levels object))))

;;; Observations
;;;

(defclass statistical-observation ()
  ((measurement-types :initform nil
		      :initarg measurement-types
		      :accessor measurement-types
		      :type sequence
		      :documentation "sequence of types corresponding
  to the classes of entries which have been measured/recorded to form
  the observation.")
   (record :initform nil
	   :initarg record
	   :accessor record
	   :type sequence
	   :documentation "the sequence of data which is a realization
  of the corresponding measurement type"))
  (:documentation "denotes a vector of measurements, not necesarily
  simple (i.e. entries could be scalar, array, network) which can be
  assumed to be independent or at least conditionally independent
  given measurements external to the collected dataset.  Failure of
  this condition implies a single observation, not multiple
  observations."))

;;; At this point, from a dataframe, which is just a simple holding
;;; structure, we should be able to extract variables and
;;; observations, which ought to be coherent, atomic, complex objects.
;;; (to create a wonderful contradiction: consider the time-series
;;; from the Dow Jones Industrial Average -- in this case, we need
;;; would have a dataset consisting of 1 observation and 1 variable --
;;; which would be the singular time series (at whatever temporal
;;; resolution was desired).

;;; For now, we need to have a means of extracting components of the
;;; dataframe into corresponding variables and observations as
;;; needed.   We don't build up the dataframe directly from variables
;;; (yet -- this could change as we consider the workflow/API
;;; approach) but rather we tear-down the dataframe through
;;; consideration of variables and observations.

;;; this naturally means that this is metadata on top of the
;;; dataframe, rather than building the dataframe on top of metadata.
;;; For pragmatic reasons, it isn't always clear that the dataframe
;;; MUST correspond to the particular instance of the practical
;;; statistical philosophy espoused in this system.  But at some more
;;; mature point, it should be.
