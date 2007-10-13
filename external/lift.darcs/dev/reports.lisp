(in-package #:lift)

;; dribble
;; full output for all tests on separate pages per suite? whatever.
;; maybe lift should have the option to print test suite names and test case names
;; test environment

#|
For *standard-input*: an input stream

For *error-output*, *standard-output*, and *trace-output*: an output stream.

For *debug-io*, *query-io*: a bidirectional stream.
|#

#|
(progn
  (setf (test-result-property *test-result* :style-sheet) "test-style.css")
  (setf (test-result-property *test-result* :title) "Test Results X")
  (setf (test-result-property *test-result* :if-exists) :supersede)
  (test-result-report *test-result*  #p"/tmp/report.html" :html))

(run-tests :suite '(lift-test test-cursors))

(run-tests :suite 'lift-test-ensure)

(test-result-property *test-result* :title)
|#

#|
in start-test (result test name)
   (push `(,name ,(current-values test)) (tests-run result))

if fails / errors, will get problem appended 

current-values comes from prototype stuff

use property-list format 
  start-time
  end-time
  time
  space??

:created
:testsuite-setup
:testing

run-tests-internal
  do-testing

do-testing (suite)
  testsuite-setup *
  foreach prototype
    initialize-test
    <fn> (= testsuite-run)
  testsuite-teardown *

testsuite-run
  foreach method in suite, run-test-internal
  if children, foreach direct-subclass, run-tests-internal

run-test-internal
  start-test - push, name, value onto test-placeholder *
  setup-test *
  lift-test *
  teardown-test *
  end-test - setf :end-time *
  (add test-data to tests-run of result)
|#

;; when it doubt, add a special
(defvar *report-environment* nil
  "Used internally by LIFT reports.")

(defun make-report-environment ()
  nil)

;; env variables need to be part saved in result

(defun test-result-report (result output format)
  (let ((*report-environment* (make-report-environment)))
    (cond ((or (stringp output)
	       (pathnamep output))
	   (with-open-file (stream 
			    output
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists (or (test-result-property
					    result :if-exists)
					   :error))
	     (%test-result-report-stream result stream format)))
	  ((streamp output)
	   (%test-result-report-stream result output format))
	  (t
	   (error "Don't know how to send a report to ~s" output)))))

(defun %test-result-report-stream (result stream format)
  (start-report-output result stream format)
  (summarize-test-result result stream format)
  (summarize-test-environment result stream format)
  (when (or (failures result) (errors result)
	    (expected-failures result) (expected-errors result))
    (summarize-test-problems result stream format))
  (summarize-tests-run result stream format)
  (end-report-output result stream format)
  (generate-detailed-reports result stream format))

(defmethod start-report-output (result stream format)
  (declare (ignore result stream format))
  )

(defmethod summarize-test-result (result stream format)
  (declare (ignore format))
  (format stream"~&Test results for: ~a~%"
	  (results-for result))
  (let ((complete-success? (and (null (errors result))
                                (null (failures result))))) 
    (cond (complete-success?
	   (format stream"~&~A Successful test~:P~%"
		   (length (tests-run result))))
	  (t
	   (format stream "~&~A Test~:P~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Error~:P~].~%"
			(length (tests-run result))
			(length (failures result))
			(length (errors result)))))))

(defmethod summarize-test-environment (result stream format)
  (declare (ignore result stream format))
  )

(defmethod summarize-test-problems (result stream format)
  (declare (ignore result stream format))
  )

(defmethod generate-detailed-reports (result stream format)
  (declare (ignore result stream format))
  )

(defmethod summarize-tests-run (result stream format)
  (declare (ignore result stream format)))

(defmethod end-report-output (result stream format)
  (declare (ignore result stream format))
  )

#+(or)
(defun summarize-test-environment (result stream format)
  (loop for symbol in (sort `((*lift-dribble-pathname*)
			      (*lift-debug-output* interpret-lift-stream)
			      (*lift-standard-output* interpret-lift-stream)
			      (*test-break-on-errors?*)
			      (*test-do-children?*)
			      (*lift-equality-test*)
			      (*test-print-length*)
			      (*test-print-level*)
			      (*lift-if-dribble-exists*))
			    'string-lessp :key 'first) do

       (print)))


;; some cruft stolen from cl-markdown
(defvar *html-meta*
  '((name (:author :description :copyright :keywords :date))
    (http-equiv (:refresh :expires))))

(defmethod start-report-output (result stream (format (eql :html)))
  (html-header 
   stream 
   (test-result-property result :title)
   (test-result-property result :style-sheet)))

(defmethod html-header (stream title style-sheet)
  (format stream "~&<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
        \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
  (format stream "~&<html>~&<head>")
  (when title
    (format stream "~&<title>~a</title>" title))
  (when style-sheet 
    (unless (search ".css" style-sheet)
      (setf style-sheet (concatenate 'string style-sheet ".css")))
    (format stream "~&<link type='text/css' href='~a' rel='stylesheet' />"
	    style-sheet))
  (format stream "~&</head>~&<body>"))

(defmethod summarize-test-result (result stream (format (eql :html)))
  (format stream "~&<div id=\"summary\">")
  (format stream "~&<h1>Test results for: ~a</h1>~%"
	  (results-for result))
  (let ((complete-success? (and (null (errors result))
                                (null (failures result))))) 
    (cond (complete-success?
	   (format stream "~&<h2>~A Successful test~:P</h2>~%"
		   (length (tests-run result))))
	  (t
	   (format stream 
		   "~&<h2>~A Test~:P~[~:;, ~:*~A Failure~:P~]~[~:;, ~:*~A Error~:P~].</h2>~%"
		   (length (tests-run result))
		   (length (failures result))
		   (length (errors result)))))

    (when (or (expected-errors result) (expected-failures result))
      (format stream "~&<h3>~[~:;~:*Expected failure~:p: ~a~]~[~:;, ~]~[~:;~:*Expected error~:p: ~a~]</h3>~%" 
	      (length (expected-failures result))
	      ;; zero if only one or the other (so we don't need a separator...)
	      (* (length (expected-failures result))
		 (length (expected-errors result)))
	      (length (expected-errors result))))

    (when (and (numberp (end-time-universal result))
	       (numberp (start-time-universal result)))
      (format stream "~&<h3>Testing took: ~:d seconds</h3>"
	      (- (end-time-universal result)
		 (start-time-universal result))))
    #+(or)
    (when (and (numberp (real-end-time result))
	       (numberp (real-start-time result)))
      (format stream "~&Time: ~,2f real-time"
	      (/ (- (real-end-time result) (real-start-time result))
		 internal-time-units-per-second))))
  (format stream "~&</div>"))

(defmethod summarize-test-environment (result stream (format (eql :html)))
  (declare (ignore result))
  (format stream "~&<div id=\"environment\">")
  
  (format stream "~&</div>"))

(defmethod summarize-test-problems (result stream  (format (eql :html)))
  (format stream "~&<div id=\"problem-summary\">")
  (format stream "~&<h2>Problem Summary:</h2>")
  (when (failures result)
    (summarize-test-problems-of-type 
     (failures result) stream "failure-summary" "Failures"))
  (when (errors result)
    (summarize-test-problems-of-type 
     (errors result) stream "error-summary" "Errors"))
  (when (expected-failures result)
    (summarize-test-problems-of-type 
     (expected-failures result)
     stream "expected-failure-summary" "Expected Failures"))
  (when (expected-errors result)
    (summarize-test-problems-of-type 
     (expected-errors result) stream "expected-failure-summary" 
     "Expected Errors"))
  (format stream "~&</div>"))

(defmethod summarize-test-problems-of-type 
    (problems stream id heading)
  (format stream "~&<div id=\"id\">" id)
  (format stream "~&<h3>~a</h3>" heading)
  (report-tests-by-suite 
   (mapcar (lambda (problem)
	     `(,(type-of (testsuite problem))
		,(test-method problem)
		(:problem ,problem)))
	   problems) stream)
  (format stream "~&</div>"))

(defmethod summarize-tests-run (result stream (format (eql :html)))
  (format stream "~&<div id=\"results\">")
  (format stream "~&<h2>Tests Run:</h2>")
  (report-tests-by-suite (tests-run result) stream)
  (format stream "~&</div>"))

(defun report-tests-by-suite (tests stream)
  (let ((current-suite nil))
    (loop for rest = (sort 
		      ;; FIXME - this is a hack intended to show tests
		      ;; in the order they were run (even if it works, it's
		      ;; bound to be fragile)
		      (copy-list tests)
		      #+(or) (nreverse (copy-list tests))
		      'string-lessp :key 'first) then (rest rest) 
       while rest
       for (suite test-name datum) = (first rest) do
       (unless (eq current-suite suite)
	 (when current-suite
	   (format stream "</div>"))
	 (setf current-suite suite)
	 (format stream "~&<div class=\"testsuite\">")
	 (let* ((this-suite-end (or 
				 (position-if 
				  (lambda (datum)
				    (not (eq current-suite (first datum))))
				  rest)
				 (length rest)))
		(error-count (count-if 
			      (lambda (datum)
				(and (getf (third datum) :problem)
				     (typep (getf (third datum) :problem)
					    'test-error)))
			      rest
			      :end this-suite-end))
		(failure-count (count-if 
				(lambda (datum)
				  (and (getf (third datum) :problem)
				       (typep (getf (third datum) :problem)
					      'test-failure)))
				rest
				:end this-suite-end))
		(extra-class (cond ((and (= error-count 0) (= failure-count 0))
				    'testsuite-all-passed)
				   ((> error-count 0)
				    'testsuite-some-errors)
				   (t
				    'testsuite-some-failures))))
	   (format stream "~&<div class=\"testsuite-title\"><table class=\"~a\"><tr><td>~a</td>" extra-class suite)
	   (format stream "<td class=\"testsuite-test-count\">~:d test~:p</td>"
		   this-suite-end)
	   (format stream "<td class=\"testsuite-summary\">")
	   (cond ((and (= error-count 0) (= failure-count 0))
		  (format stream "all passed"))
		 (t
		  (format stream "~[~:;~:*~:d failure~:p~]" 
			  failure-count)
		  (when (and (> error-count 0) (> failure-count 0))
		    (format stream ", "))
		  (format stream  "~[~:;~:*~a error~:p~]" 
			  error-count)))
	   (format stream "</td></tr></table>")
	   (format stream "</div>")))
	 (format stream "~&<div class=\"test-case\">")
	 (let ((problem (getf datum :problem)))
	   (cond ((typep problem 'test-failure)
		  (format stream "~&<span class=\"test-name\"><a href=\"~a\" title=\"details\">~a</a></span>"
			  (details-link stream suite test-name)
			  test-name)
		  (format stream 
			  "~&<span class=\"test-failure\">failure</span>" ))
		 ((typep problem 'test-error)
		  (format stream "~&<span class=\"test-name\"><a href=\"~a\" title=\"details\">~a [during ~a]</a></span>"
			  (details-link stream suite test-name)
			  test-name
			  (test-step problem))
		  (format stream "~&<span class=\"test-error\">error</span>"))
		 (t
		  (format stream "~&<span class=\"test-name\">~a</span>" 
			  test-name)
		  (let ((seconds (getf datum :seconds))
			(conses (getf datum :conses)))
		    (when seconds 
		      (format stream "<span class=\"test-time\">~,3f</span>"
			      seconds))
		    (when conses 
		      (format stream "<span class=\"test-space\">~:d</span>"
			      conses)))))
	   (format stream "~&</div>")))
    (when current-suite
      (format stream "</div>"))))

(defun get-details-links-table ()
  (let ((hash (getf *report-environment* :details-links)))
    (or hash
	(setf (getf *report-environment* :details-links)
	      (make-hash-table :test 'equal)))))

#+(or)
(get-details-links-table)

(defun details-link (stream suite name)
  (declare (ignore stream))
  (let* ((hash (get-details-links-table)))
    (or (gethash (cons suite name) hash)
	(progn
	  (incf (getf *report-environment* :details-links-count 0))
	  (setf (gethash (cons suite name) hash)
		(make-pathname 
		 :name (format nil "details-~a" 
			       (getf *report-environment* :details-links-count))
		 :type "html"))))))

(defmethod end-report-output (result stream (format (eql :html)))
  (let ((style-sheet (test-result-property result :style-sheet)))
    (when style-sheet
      (ignore-errors
	(copy-file (asdf:system-relative-pathname 
		    'lift "resources/test-style.css")
		   (make-pathname 
		    :name (pathname-name style-sheet)
		    :type (pathname-type style-sheet)
		    :defaults (pathname stream))
		   :if-exists :supersede))))
  (html-footer stream))

(defun html-footer (stream)
  (format stream "<div id=\"footer\">")
  (format stream "~&generated on ~a" 
	  #+allegro
	  (excl:locale-print-time 
	   (get-universal-time)
	   :fmt "%B %d, %Y %T GMT%z" :stream nil)
	  #-allegro
	  (get-universal-time))
  (format stream "</div>")
  (format stream "~&</body></html>"))

(defmethod generate-detailed-reports (result stream (format (eql :html)))
  (loop for (suite test-name datum)  in (tests-run result)
     when (getf datum :problem) do
     (let ((output-pathname (merge-pathnames
			     (details-link stream suite test-name) 
			     stream)))
       (ensure-directories-exist output-pathname)
       (with-open-file (out output-pathname
			    :direction :output
			    :if-does-not-exist :create
			    :if-exists :supersede)
	 (html-header 
	  out 
	  (format nil "Test ~a details | ~a" 
		  test-name (test-result-property result :title))
	  (test-result-property result :style-sheet))
	 (format out "~&<h2>Test ~a details</h2>" test-name)
	 (format out "~&<a href=\"~a\">Back</a>"
		 (namestring (make-pathname :name (pathname-name stream)
					    :type (pathname-type stream))))
	 (format out "~&<pre>")
	 (format out "~a"
		 (encode-pre 
		  (with-output-to-string (s)
		    (print-test-problem "" (getf datum :problem) s))))
	 (format out "~&</pre>") 
	 (html-footer out)))))

#+(or)
(defmethod summarize-test-environment (result stream format)
  (loop for symbol in (sort `((*lift-dribble-pathname*)
			      (*lift-debug-output* interpret-lift-stream)
			      (*lift-standard-output* interpret-lift-stream)
			      (*test-break-on-errors?*)
			      (*test-do-children?*)
			      (*lift-equality-test*)
			      (*test-print-length*)
			      (*test-print-level*)
			      (*lift-if-dribble-exists*))
			    'string-lessp :key 'first) do

       (print)))

(defun encode-pre (string)
  ;; Copied from CL-Markdown
  ;; Copied from HTML-Encode
  ;;?? this is very consy
  ;;?? crappy name
  (declare (simple-string string))
  (let ((output (make-array (truncate (length string) 2/3)
                            :element-type 'character
                            :adjustable t
                            :fill-pointer 0)))
    (with-output-to-string (out output)
      (loop for char across string
	 do (case char
	      ((#\&) (write-string "&amp;" out))
	      ((#\<) (write-string "&lt;" out))
	      ((#\>) (write-string "&gt;" out))
	      (t (write-char char out)))))
    (coerce output 'simple-string)))

;;;;;

(defmethod summarize-test-result (result stream (format (eql :describe)))
  (describe result stream))

(defmethod summarize-tests-run (result stream (format (eql :describe)))
  (declare (ignore result stream))
  )
  
;;;;;

(defmethod summarize-test-result (result stream (format (eql :save)))
  (flet ((add-property (name)
	   (format stream "~&\(~s ~a\)" 
		   (intern (symbol-name name) :keyword)
		   (slot-value result name))))
    (format stream "\(~%")
    (add-property 'results-for)
    (format stream "~&\(:date-time ~a\)" (get-universal-time))
    (add-property 'real-start-time-universal)
    (add-property 'start-time-universal)
    (add-property 'end-time-universal)
    (add-property 'real-end-time-universal)
    (format stream "~&\(:tests-run ")
    (loop for (suite name data) in
       ;; FIXME - this is a hack intended to show tests
       ;; in the order they were run (even if it works, it's
       ;; bound to be fragile)
	 (copy-list (tests-run result))
	 #+(or)
	 (nreverse (copy-list (tests-run result))) do
	 (labels ((out (name &key (source data)
			     (print-if-nil? nil))
		    (let* ((key (intern (symbol-name name) :keyword))
			   (value (getf source key))) 
		      (when (or value print-if-nil?)
			(format stream "~&\(~s ~a\)" key value))))
		  (prop (name)
		    (out name :source (getf data :properties))))
	   (format stream "\(~%")
	   (format stream "~&\(:suite ~a\)" suite)	     
	   (format stream "~&\(:name ~a\)" name)
	   ;; FIXME - we could make these extensible
	   (out 'start-time-universal)
	   (out 'end-time-universal)
	   (out 'result)
	   (out 'seconds)
	   (out 'conses)
	   (loop for stuff in (getf data :properties) by #'cddr do
		(prop stuff))
	   (format stream "~&\)")))
    (format stream "~&\)")
    (format stream "~&\)")))
  
#+(or)
(compile 'summarize-test-result)

#+(or)
(progn
  (setf (test-result-property *test-result* :if-exists) :supersede)
  (test-result-report *test-result* #p"/tmp/report.save" :save))

(defun symbol->turtle (symbol)
  (let ((upcase? nil))
    (coerce
     (loop for char across (string-downcase (symbol-name symbol)) 
	when (char= char #\-) do (setf upcase? t)
	else collect (if upcase? 
			 (prog1 (char-upcase char) 
			   (setf upcase? nil))
			 char))
     'string)))

(defun turtlefy (thing)
  (typecase thing
    (string thing)
    (pathname (namestring thing))
    (number 
     (etypecase thing
       (integer (format nil "\"~a\"^^xsd:integer" thing))
       (double-float (format nil "\"~f\"^^xsd:double" thing))
       (single-float (format nil "\"~f\"^^xsd:single" thing))))
    (symbol (symbol-name thing))
    (t (format nil "\"~a\"" thing))))

(defun ensure-symbol (thing)
  (etypecase thing
    (symbol thing)
    (string (intern thing))))

#+(or)
(symbol->turtle 'real-start-time-universal)

(defun date->turtle (&key (datetime (get-universal-time)) (include-time? nil))
  (multiple-value-bind
	(second minute hour day month year day-of-the-week)
      (decode-universal-time datetime)
    (declare (ignore day-of-the-week))
    (let ((date-part (format nil "~d-~2,'0d-~2,'0d" year month day))
	  (time-part (and include-time? 
			  (format nil "T-~2,'0d:~2,'0d:~2,'0d"
					hour minute second)))
	  (data-type (if include-time?
			 "xsd:dateTime" "xsd:date")))
      (concatenate 'string "\"" date-part time-part  "\"" "^^" data-type))))

;; http://www.dajobe.org/2004/01/turtle/
(defmethod summarize-test-result (result stream (format (eql :turtle)))
  (labels ((convert-value (value type)
	     (ecase type
	       (string (turtlefy value))
	       (symbol (ensure-symbol value))
	       (date (date->turtle :datetime value))
	       (dateTime (date->turtle :datetime value :include-time? t))))
	   (add-property (name type)
	     (let ((value (slot-value result name)))
	       (when value
		 (format stream "~&:~a ~s ;" 
			 (symbol->turtle name)
			 (convert-value value type))))))
    (format stream 
	    "~&@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .~%")
    (format stream 
	    "~&@prefix : <http://www.metabang.com/2007/04/lift#> .~%")
    (format stream "\[~%")
    (add-property 'results-for 'string)
    (add-property 'real-start-time 'dateTime)
    (add-property 'start-time 'dateTime)
    (add-property 'end-time 'dateTime)
    (add-property 'real-end-time 'dateTime)
    (format stream "~&\:testsRun (")
    (loop for (suite name data) in 
       ;; FIXME - this is a hack intended to show tests
       ;; in the order they were run (even if it works, it's
       ;; bound to be fragile)
	 (copy-list (tests-run result))
	 #+(or)
	 (nreverse (copy-list (tests-run result))) do
	 (labels ((out (name type &key (source data))
		    (let* ((key (intern (symbol-name name) :keyword))
			   (value (getf source key)))
		      (when value
			(format stream "~&  :~a ~a ;" 
				(symbol->turtle name)
				(convert-value value type)))))
		  (prop (name type)
		    (out name type :source (getf data :properties))))
	   (format stream "~&\[ ")
	   (format stream ":testSuite ~s ;" (symbol-name suite))
	   (format stream "~&  :testName ~s ;" (symbol-name name))
	   ;; FIXME - we could make these extensible
	   (out 'start-time 'dateTime)
	   (out 'end-time 'dateTime)
	   (out 'result 'string)
	   (out 'seconds 'string)
	   (out 'conses 'string)
	   (loop for stuff in (getf data :properties) by #'cddr do
		(prop stuff 'string))
	   (format stream " \]")))
    (format stream " ) ~&\] . ")))
  
#+(or)
(progn
  (setf (test-result-property *test-result* :if-exists) :supersede)
  (test-result-report *test-result*  #p"/tmp/report.n3" :turtle))
