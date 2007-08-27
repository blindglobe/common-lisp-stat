;;
;; Copyright (c) 2006, Cyrus Harmon
;;
;; filter.lisp - filters are used to process markup document
;; sexps. The intent is that these filters are independent from the
;; final rendering. Think of this as a set of preprocessing stages
;; after document parsing. So now we have a markup document which gets
;; parsed, filtered and then rendered.
;;
;; Yes, perhaps the name "filter" could be better. I'm open to
;; suggestions.

(in-package :smarkup)

(defgeneric filter-gf (filter car list))
(defgeneric filter (type sexp))

(defmethod filter-gf (filter car list)
  (cond ((null car) (when (cdr list) (filter filter (cdr list))))
        ((atom car) (cons car (filter filter (cdr list))))
        (t (cons (filter filter (car list))
                 (filter filter (cdr list))))))

(defmethod filter (type sexp)
  (filter-gf type (car sexp) sexp))

(defun apply-filters (sexp filters)
  (cond ((null filters) sexp)
        ((listp filters) (apply-filters (filter (car filters) sexp) (cdr filters)))
        (t (filter filters sexp))))

;;;
;;; test filter. can reverse strings
;;;
(defun reverse-strings (sexp)
  (cond ((null sexp) nil)
        ((atom sexp) (if (stringp sexp)
                         (reverse sexp)
                         sexp))
        (t (cons (reverse-strings (car sexp))
                 (reverse-strings (cdr sexp))))))

(defmethod filter-gf ((filter (eql :test)) (car (eql :reverse)) list)
  `(:span ,@(filter :test (reverse-strings (reverse (cdr list))))))

;;;
;;; lisp filter. can eval lisp forms
;;;
(defun collect-string (str-list)
  (declare (optimize (debug 2)))
  (apply #'concatenate 'string
         (mapcar
          #'(lambda (x)
              (unless (equal x 'blank-line)
                (format nil "~A" x)))
          str-list)))

(defun lc-format (dest ctrl-string &rest args)
  (let ((*print-case* :downcase))
    (apply #'format dest ctrl-string args)))

(defun eval-lisp (tag body &key (show-commands t) (show-results t))
  (declare (ignore tag))
  (let ((lines
         (with-input-from-string (ifs (collect-string body))
           (loop for line = (read ifs nil)
              while line collect line))))
    (if (not (or show-commands show-results))
        (progn (mapcar #'eval lines)
               nil)
        `((:div :class "lisp")
          (:pre
           ,@(mapcan #'(lambda (x)
                         (cons `(:code 
                                 ,(when show-commands
                                        (lc-format nil "~W~%" x)))
                               (let ((output (eval x)))
                                 (if show-results
                                     (list `(:results
                                             ,(if (stringp output)
                                                  (lc-format nil "~W~%" output)
                                                  (format nil "~S~%" output)))
                                           #\Newline)
                                     (list #\Newline)))))
                     lines))))))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :lisp)) list)
  (eval-lisp car (cdr list)))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :lisp-no-results)) list)
  (eval-lisp car (cdr list) :show-results nil))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :lisp-no-commands)) list)
  (eval-lisp car (cdr list) :show-commands nil))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :lisp-silent)) list)
  (eval-lisp car (cdr list) :show-results nil :show-commands nil))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :lisp-no-eval)) list)
  `((:div :class "lisp") (:pre ,@(cdr list))))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :lisp-value)) list)
  (eval (read-from-string (car (cdr list)))))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :setf-lisp-value)) list)
  (let ((sexp (read-from-string (car (cdr list)))))
    (setf (symbol-value (car sexp)) (cadr sexp))
    nil))

(defmethod filter-gf ((filter (eql :lisp)) (car (eql :code-block)) list)
  `((:div :class "lisp") (:pre ,@(cdr list))))

;;;
;;; markup-metadata filter. sets various special variables with
;;; document metadata info
;;;
(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :copyright)) list)
  (setf *copyright* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :title)) list)
  (setf *document-title* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :titlerunning)) list)
  (setf *document-titlerunning* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :subtitle)) list)
  (setf *document-subtitle* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :author)) list)
  (setf *document-author* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :tocauthor)) list)
  (setf *document-tocauthor* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :authorrunning)) list)
  (setf *document-authorrunning* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :address)) list)
  (setf *document-address* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :institute)) list)
  (setf *document-institute* (cadr list))
  (call-next-method))

(defparameter *bibtex-macros* (make-hash-table :test #'equalp))
(defparameter *bibtex-database* (make-hash-table :test #'equalp))
(defparameter *bibtex-style* nil)
(defparameter *cite-keys* nil)
(defparameter *cite-order* (make-hash-table :test #'equalp))
(defparameter *current-citation* 0)

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :bibtex-database)) list)
  (setf *bibtex-database* (make-hash-table :test #'equalp))
  (setf *cite-keys* nil)
  (setf *cite-order* (make-hash-table :test #'equalp))
  (setf *current-citation* 0)
  (cond ((and (stringp (cadr list))
              (eql (char (cadr list) 0) #\())
         (let ((database-spec-list
                (let ((*read-eval* nil))
                  (read-from-string (cadr list)))))
           (loop for database-spec in database-spec-list
              do
                (let ((database-uri (puri:parse-uri database-spec)))
                  (cond ((eql (puri:uri-scheme database-uri) :asdf)
                         (let ((database
                                (asdf:component-pathname
                                 (ch-asdf::asdf-lookup database-spec))))
                           (let ((bibtex-runtime::*bib-database* *bibtex-database*)
                                 (bibtex-runtime::*bib-macros* *bibtex-macros*))
                             (with-open-file (f database)
                               (bibtex-runtime:read-bib-database f)))))
                        (t
                         (let ((database (puri:uri-path database-uri)))
                           (let ((bibtex-runtime::*bib-database* *bibtex-database*)
                                 (bibtex-runtime::*bib-macros* *bibtex-macros*))
                             (with-open-file (f database)
                               (bibtex-runtime:read-bib-database f)))))))))
         (call-next-method))
        (t (warn "Unsupported bibilography database~&"))))

(defmethod filter-gf ((filter (eql :smarkup-metadata))
                      (car (eql :bibtex-style))
                      list)
  (let ((bst (cadr list)))
    (setf *bibtex-style* bst)))

(defmethod filter-gf ((filter (eql :smarkup-metadata)) (car (eql :smarkup-metadata)) list)
  (filter-gf filter (cadr list) (cdr list))
  nil)

(defparameter *html-css-stylesheet-url* "style.css")

(defmethod filter-gf ((filter (eql :html-metadata)) (car (eql :htmlcss)) list)
  (setf *html-css-stylesheet-url* (cadr list))
  (call-next-method))

(defmethod filter-gf ((filter (eql :html-metadata)) (car (eql :html-metadata)) list)
  (filter-gf filter (cadr list) (cdr list))
  nil)



;;;
;;; references
;;;


;;;
;;; Ok, need to get the bibtex-style and then call bibtex or its guts to get the references.
;;;

(defun get-bib-entry (entry)
  (with-output-to-string (stream)
    (bibtex-runtime::write-bib-entry entry stream)))

(defun get-bib-order (entry)
  (gethash entry *cite-order*))

(defmethod filter-gf ((filter (eql :ref)) (car (eql :bibcite)) list)
  (loop for  cite-key in (cdr list)
     do
       (unless (member cite-key *cite-keys* :test 'string=)
         (push cite-key *cite-keys*)
         (setf (gethash cite-key *cite-order*) (incf *current-citation*)))
       (let ((v (gethash cite-key *bibtex-database*)))
         (if v
             (bibtex-runtime::write-bib-entry v)
             (warn "~%bibliography entry ~A not found~%" cite-key))))
  (call-next-method))


;;;
;;; outline stuff
;;;

(defparameter *outline-elements* '(:h1 :h2 :h3 :h4 :h5 :h6))

(defparameter *outline-level* 6)

(defmethod filter-gf ((filter (eql :outline)) car list)
  (let ((outline-elements
         (subseq *outline-elements*
                 0 (min (length *outline-elements*)
                        *outline-level*))))
    (remove-if-not #'(lambda (x) (member (car x) outline-elements))
                   list)))

