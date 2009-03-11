;;;
;;; $Header: /home/gene/library/website/docsrc/dsv/RCS/dsv.lisp,v 395.1 2008/04/20 17:25:46 gene Exp $
;;;
;;; Copyright (c) 2005 Gene Michael Stover.  All rights reserved.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
;;; USA

;;; This package provides tools for reading a DSV file on a stream and
;;; returning a list of lists of strings, in row-major form ("list of
;;; rows" format).   One needs to convert these strings to something
;;; useful at the end if strings are not useful.

(defpackage :cybertiggyr-dsv
  (:use :common-lisp)
  (:import-from :cybertiggyr-test "DEFTEST")
  (:export "*END-OF-RECORD*"
	   "*ESCAPE*"
	   "*FIELD-SEPARATOR*"
           "DO-ESCAPED"
	   "LOAD-ESCAPED"
	   "READ-ESCAPED"))

(in-package :cybertiggyr-dsv)

;;;
;;; UNEXPORTED HELPER FUNCTIONS & STOOF
;;;

(defun xpeek (strm)
  "Return the next character without consuming it, or return STRM on
end-of-input or other error."
  (peek-char nil strm nil strm))

(defun consume-leading-crap (strm crap)
  "Read (consume) newlines until the next character is not a newline or
there is no next character (end-of-input, which isn't an error)."
    (loop while (eql (xpeek strm) crap) do (read-char strm))
  'consume-leading-crap)

(defun read-escaped-field (strm terminators escape)
  "Return the next field as a string.  Return STRM if there is no next
field, which is when the stream is already at its end.  Assumes caller
has already consumed white-space crap that might preceed the field.
Consumes the character which ends the field.  TERMINATORS is a list
of characters & the stream which could terminate the field."
  (if (eq (xpeek strm) strm)
      strm                              ; already at end-of-input
      ;; else, Consume & collect characters until we find a terminator (field
      ;; terminator, record terminator, or end-of-input).  Do not collect
      ;; the terminator.
      (coerce
       (loop
	  until (member (xpeek strm) terminators)
	  collect (if (eql (xpeek strm) escape)
		      ;; It's an escape, so discard it & use the next
		      ;; character, verbatim.
		      (progn
			(read-char strm)
			(read-char strm))
		      ;; else, Use this character.
		      (read-char strm)))
       'string)))

;;;
;;;
;;;

(defvar *field-separator* #\:
  "The default field separator character.  It defaults to colon (:).")

(defvar *end-of-record* #\Newline
  "The end-of-record character.  Defaults to Newline.")

(defvar *escape* #\\
  "The default escape character for unix-style DSV files.  It uses a single
escape character to allow the field separator character to occur
within fields.  The escape character can be used to allow an end-of-line
character or an escape character to occur in fields, too.
Defaults to backslash (\\).  You can change it with SETQ.  If you do not
want to allow separator characters at all, bind it to NIL.")

(defun read-escaped (strm &key (field-separator *field-separator*)
			  (end-of-record *end-of-record*)
			  (escape *escape*))
  "Read (consume) & return the next DSV record from STRM.  The record
will be a list of fields.  The fields will be strings.  Field separator
& end-of-record characters may not occur within fields unless escaped.
If you don't want to allow any kind of escape, use NIL for the escape
character.  Since NIL is not a character, it will never be equal to a
character read from STRM, so there will be no possible escape character.
In fact, you could use any non-character to disable the escape
character.  Ignors empty lines.  On end-of-input, returns STRM.  It is
an error if an escape character is followed by end-of-input.

<example broken=TRUE>
  (with-open-stream (s (make-stream-from-file \"test.csv\"))
     (loop (read-escaped s) collecting result))
</example>
"
  (consume-leading-crap strm end-of-record)
  (if (eq (xpeek strm) strm)
      strm				; normal end-of-input
    ;; else, Let's collect fields until we have read an entire record.
    (prog1
	(loop until (member (xpeek strm) (list strm end-of-record))
	      collect (prog1
			  (read-escaped-field strm
					      (list strm field-separator
						    end-of-record)
					      escape)
			(when (eql (xpeek strm) field-separator)
			  ;; Consume the character which ended the field.
			  ;; Notice that we do not consume end-of-record
			  ;; characters.
			  (read-char strm))))
      (consume-leading-crap strm end-of-record))))

(defun load-escaped (pathname &key (field-separator *field-separator*)
			      (end-of-record *end-of-record*)
			      (escape *escape*)
			      (filter #'identity)
			      (trace nil))
  "Return the entire contents of an escaped DSV file as a list of
records.  Each record is a list."
  (with-open-file (strm pathname :direction :input)
    (labels ((is-good (x) (funcall filter x))
	     (xread () (read-escaped strm :field-separator field-separator
				     :end-of-record end-of-record
				     :escape escape)))
      (do ((lst () (if (is-good x) (cons x lst) lst)) ;second (is-good x) was x
	   (x (xread) (xread))
	   (i 0 (1+ i)))
	  ((eq x strm) lst)
	  (when (and trace (zerop (mod i 1000)))
	    (format trace "~&~A: [~D] ~S" 'load-escaped i x))))))

;;;
;;; todo: new new new.  Document me!!!
;;;
(defmacro do-escaped ((var pathname) &body body)
  (let ((strm (gensym)))
    `(with-open-file (,strm ,pathname :element-type 'character
			    :direction :input :if-does-not-exist :error)
       (loop for ,var = (read-escaped ,strm)
	     while (not (eq ,var ,strm))
	     do (progn ,@body))
       (truename ,pathname))))

;;;
;;; TESTS
;;;

(deftest test0000 ()
  "Null test.  Always succeeds."
  'test0000)

(deftest test0010 ()
  "Test that XPEEK returns the correct character from a stream, does
not consume the character.  The character is NOT the last in the stream."
  (with-input-from-string (strm "abc")
    (and (eql (xpeek strm) #\a)
	 (eql (read-char strm) #\a))))

(deftest test0011 ()
  "Like TEST0011 except that it tests XPEEK on the last character in the
stream.  In other words, tests that XPEEK returns the correct value &
does not consume it, & that character is the last in the stream."
  (with-input-from-string (strm "c")
    (and (eql (xpeek strm) #\c)
	 (eql (read-char strm) #\c))))

(deftest test0012 ()
  "Test XPEEK on an empty stream."
  (with-input-from-string (strm "")
    (and (eq (xpeek strm) strm)
	 (eq (read-char strm nil strm) strm))))

(deftest test0015 ()
  "Test CONSUME-LEADING-CRAP on a stream that contains nothing but leading
crap."
  (with-input-from-string (strm (format nil "~%~%~%"))
    (and (eql (xpeek strm) #\Newline)   ; not at end
	 (consume-leading-crap strm #\Newline) ; doesn't matter what it returns
	 (eq (read-char strm nil strm) strm)))) ; now we're at end

(deftest test0016 ()
  "Test CONSUME-LEADING-CRAP on a streeam that starts with leading crap,
then has some non-crap."
  (with-input-from-string (strm (format nil "~%~%~%a"))
    (and (eql (xpeek strm) #\Newline)   ; not at end
	 (consume-leading-crap strm #\Newline) ; doesn't matter what it returns
	 (eql (read-char strm) #\a))))

(deftest test0017 ()
  "Test CONSUME-LEADING-CRAP on a stream that starts with non-crap, then
has some crap.  CONSUME-LEADING-CRAP should not consume the leading
non-crap."
  (with-input-from-string (strm (format nil "a~%"))
    (and (eql (xpeek strm) #\a)         ; not at end
	 (consume-leading-crap strm #\Newline) ; doesn't matter what it returns
	 (eql (read-char strm) #\a))))  ; the "a" char should remain

(deftest test0020 ()
  "Test READ-ESCAPED-FIELD on a stream that contains a single field
followed by end-of-input.  Uses the default field separator, end-of-record
character, & escape character.  Just test that the field is read, not that
the next READ-ESCAPED-FIELD indicates end-of-input."
  (with-input-from-string (strm "abc")
    (equal (read-escaped-field strm
			       (list strm *field-separator* *end-of-record*)
			       *escape*)
	   "abc")))

(deftest test0021 ()
  "Like TEST0020, but also checks that another call to READ-ESCAPED-FIELD
indicates end-of-input by returning STRM."
  (with-input-from-string (strm "abc")
    (let* ((a (read-escaped-field strm
				  (list strm *field-separator* *end-of-record*)
				  *escape*))
	   (b (read-escaped-field strm
				  (list strm *field-separator* *end-of-record*)
				  *escape*)))
      (unless (equal a "abc")
	(format t "~&~A: First read should have returned" 'test0021)
	(format t " ~S, but it returned ~S" "abc" a))
      (unless (eq b strm)
	(format t "~&~A: Second read should have returned" 'test0021)
	(format t " ~S, but it returned ~S" strm b))
      (and (equal a "abc") (eq b strm)))))

(deftest test0025 ()
  "Test that READ-ESCAPED-FIELD works on two consecutive fields."
  (let ((a "abc") (b "xyz"))
    (with-input-from-string (strm (format nil "~A~A~A" a *field-separator* b))
      (let* ((terminators (list strm *field-separator* *end-of-record*))
	     (xa (read-escaped-field strm terminators *escape*))
	     (xseparator (read-char strm))
	     (xb (read-escaped-field strm terminators *escape*))
	     (xstrm (xpeek strm)))
	(and (equal xa a) (eql xseparator *field-separator*) (equal xb b)
	     (eq xstrm strm))))))

(deftest test0026 ()
  "Test that READ-ESCAPED-FIELD works on two records of two fields each.
The second record does not end with an end-of-record character.  It
ends with end-of-input on the stream."
  (let* ((a "abc") (b "123")            ; first record
	 (c "def") (d "456")            ; second record
	 (string (format nil "~A~A~A~A~A~A~A" a *field-separator* b
			 *end-of-record* c *field-separator* d)))
    (with-input-from-string (strm string)
      (let* ((terminators (list strm *field-separator* *end-of-record*))
	     (xa (read-escaped-field strm terminators *escape*))
	     (xseparator0 (read-char strm))
	     (xb (read-escaped-field strm terminators *escape*))
	     (xend-of-record0 (read-char strm))
	     (xc (read-escaped-field strm terminators *escape*))
	     (xseparator1 (read-char strm))
	     (xd (read-escaped-field strm terminators *escape*))
	     (xstrm (xpeek strm)))
	(and (equal xa a)
	     (eql xseparator0 *field-separator*)
	     (equal xb b)
	     (eql xend-of-record0 *end-of-record*)
	     (equal xc c)
	     (eql xseparator1 *field-separator*)
	     (equal xd d)
	     (eq xstrm strm))))))

(deftest test0027 ()
  "Like TEST0026 except that the second record ends with an end-of-
record character."
  (let* ((a "abc") (b "123")            ; first record
	 (c "def") (d "456")            ; second record
	 (string (format nil "~A~A~A~A~A~A~A~A" a *field-separator* b
			 *end-of-record* c *field-separator* d
			 *end-of-record*)))
    (with-input-from-string (strm string)
      (let* ((terminators (list strm *field-separator* *end-of-record*))
	     (xa (read-escaped-field strm terminators *escape*))
	     (xseparator0 (read-char strm))
	     (xb (read-escaped-field strm terminators *escape*))
	     (xend-of-record0 (read-char strm))
	     (xc (read-escaped-field strm terminators *escape*))
	     (xseparator1 (read-char strm))
	     (xd (read-escaped-field strm terminators *escape*))
	     (xend-of-record1 (read-char strm))
	     (xstrm (xpeek strm)))
	(and (equal xa a)
	     (eql xseparator0 *field-separator*)
	     (equal xb b)
	     (eql xend-of-record0 *end-of-record*)
	     (equal xc c)
	     (eql xseparator1 *field-separator*)
	     (equal xd d)
	     (eql xend-of-record1 *end-of-record*)
	     (eq xstrm strm))))))

(deftest test0050 ()
  "Test READ-ESCAPED on an input stream containing a single record of a
single field."
  (let* ((record (list "abc"))
	 (string (format nil "~A" (first record))))
    (with-input-from-string (strm string)
      (let* ((xrecord (read-escaped strm))
	     (xstrm (xpeek strm)))
	(and (equal xrecord record)
	     (eq xstrm strm))))))

(deftest test0051 ()
  "Test READ-ESCAPED on an input stream containing a single record of two
fields."
  (let* ((record (list "abc" "123"))
	 (string (format nil "~A~A~A" (first record) *field-separator*
			 (second record))))
    (with-input-from-string (strm string)
      (let* ((xrecord (read-escaped strm))
	     (xstrm (xpeek strm)))
	(and (equal xrecord record)
	     (eq xstrm strm))))))

(deftest test0052 ()
  "Test READ-ESCAPED.  After reading the single record of two fields,
the stream should be at its end.  The record is followed by several
end-of-record characters, & the stream should be at its end after
reading the record because no records follow the record terminators."
  (let* ((record (list "abc" "123"))
	 (string (format nil "~A~A~A~A~A~A" (first record) *field-separator*
			 (second record) *end-of-record* *end-of-record*
			 *end-of-record*)))
    (with-input-from-string (strm string)
      (let* ((xrecord (read-escaped strm))
	     (xstrm (xpeek strm)))
	(and (equal xrecord record)
	     (eq xstrm strm))))))

(deftest test0053 ()
  "Test READ-ESCAPED on an input of two, two-field records.  The second
record is followed by one end-of-record character."
  (let ((record0 '("aaa" "111"))
	(record1 '("bbb" "222"))
	(string (format nil "aaa~A111~Abbb~A222~A"
			*field-separator* *end-of-record*
			*field-separator* *end-of-record*)))
    (with-input-from-string (strm string)
      (let* ((xrecord0 (read-escaped strm))
	     (xrecord1 (read-escaped strm)))
	(unless (equal xrecord0 record0)
	  (format t "~&First record is ~S.  Expected ~S." xrecord0 record0))
	(unless (equal xrecord1 record1)
	  (format t "~&Second record is ~S.  Expected~S." xrecord1 record1))
	(and (equal xrecord0 record0)
	     (equal xrecord1 record1))))))

;;; --- end of file ---

