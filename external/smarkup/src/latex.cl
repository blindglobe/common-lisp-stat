;;
;; Copyright (c) 2006, Cyrus Harmon
;;
;; latex.cl - latex output from smarkup
;;

(in-package :smarkup)

(defvar *baseline-skip* "12pt")
(defvar *baseline-stretch* "1.6")
(defvar *par-skip* "18pt")
(defvar *latex-graphics-params* nil)

(defparameter *document-format-parameters*
  '(#+nil ("oddsidemargin" . "0.5in")
    #+nil ("textwidth" . "6.0in")
    #+nil ("topmargin" . "0in")
    #+nil ("headheight" . "0.1in")
    #+nil ("headsep" . "0.0in")
    #+nil ("textheight" . "9.6in")
    #+nil ("footskip" . "0.4in")
    #+nil ("parindent" . "0.5in")))

(defvar *section-numbering-depth* 5)

(defparameter *document-class* "article")
(defparameter *document-options* '("10pt"))

(defvar *document-latex-commands*
  '("\\newcommand{\\argmax}{\\operatornamewithlimits{argmax}}"
    "\\newcommand{\\argmin}{\\operatornamewithlimits{argmin}}"))

(defparameter *latex-packages*
  '("amssymb" "amsmath" "verbatim" "graphicx" "subfigure"
    "caption" "hyperref" "fancyheadings" "longtable"
    ("geometry" . "letterpaper")))

(defparameter *thesis-preamble*
  "\\DeclareCaptionFont{singlespacing}{\\ssp}
\\captionsetup{font={singlespacing,small}}")

;;; \\captionsetup{font={singlespacing,small}}

(defparameter *beamer-preamble*
  "\\mode<presentation>{
\\definecolor{nicegreen}{RGB}{10,100,10}
\\setbeamercolor*{normal text}{bg=black,fg=white}
\\setbeamercolor{structure}{fg=nicegreen}
}
")


(defparameter *res-preamble* "
\\oddsidemargin -.5in
\\evensidemargin -.5in
\\textwidth=6.0in
\\itemsep=0in
\\parsep=0in
\\parskip=6pt
\\topmargin=-.4in
\\textheight=60\\baselineskip
\\hyphenpenalty=5000
\\tolerance=1000

\\newenvironment{list1}{
  \\begin{list}{}{%
      \\setlength{\\itemsep}{0in}
      \\setlength{\\parsep}{0in} \\setlength{\\parskip}{0in}
      \\setlength{\\topsep}{0in} \\setlength{\\partopsep}{0in} 
      \\setlength{\\leftmargin}{0.17in}}}{\\end{list}}
\\newenvironment{list2}{
  \\begin{list}{$\\bullet$}{%
      \\setlength{\\itemsep}{0in}
      \\setlength{\\parsep}{0in} \\setlength{\\parskip}{0in}
      \\setlength{\\topsep}{0in} \\setlength{\\partopsep}{0in} 
      \\setlength{\\leftmargin}{0.2in}}}{\\end{list}}")

(defparameter *llncs-preamble*
  "\\bibliographystyle{splncs}
\\renewcommand\\floatpagefraction{.9}
\\renewcommand\\topfraction{.9}
\\renewcommand\\bottomfraction{.9}
\\renewcommand\\textfraction{.1}   
\\setcounter{totalnumber}{50}
\\setcounter{topnumber}{50}
\\setcounter{bottomnumber}{50}
")

(defparameter *acm-proc-article-preamble*
  "\\bibliographystyle{splncs}
\\renewcommand\\floatpagefraction{.95}
\\renewcommand\\topfraction{.95}
\\renewcommand\\bottomfraction{.95}
\\renewcommand\\textfraction{.1}   
\\setcounter{totalnumber}{50}
\\setcounter{topnumber}{50}
\\setcounter{bottomnumber}{50}
\\widowpenalty=10000
\\clubpenalty=10000
\\raggedbottom
")

(defparameter *article-preamble*
  "\\setcounter{topnumber}{2}
\\setcounter{bottomnumber}{2}
\\setcounter{totalnumber}{4}     % 2 may work better
\\setcounter{dbltopnumber}{2}    % for 2-column pages
\\renewcommand{\\dbltopfraction}{0.9}	% fit big float above 2-col. text
\\renewcommand{\\textfraction}{0.07}	% allow minimal text w. figs
%   Parameters for FLOAT pages (not text pages):
\\renewcommand{\\floatpagefraction}{0.7}	% require fuller float pages
% N.B.: floatpagefraction MUST be less than topfraction !!
\\renewcommand{\\dblfloatpagefraction}{0.7}	% require fuller float pages
\\setlength{\\captionmargin}{10pt}")

(defvar *article-headings* '((:h1 . "section")
                                   (:h2 . "subsection")
                                   (:h3 . "subsubsection")
                                   (:h4 . "paragraph")))

(defvar *thesis-headings* '((:h1 . "chapter")
                                  (:h2 . "section")
                                  (:h3 . "subsection")
                                  (:h4 . "subsubsection")))

(defparameter *thesis-approval-page* t)

(defun latex-command (command &optional arg)
  (format nil "~&\\~A~@[{~A}~]~%" command arg))

(defgeneric emit-latex (stream thing &key newline))
(defgeneric emit-latex-gf (stream type children &key newline))

;;; default is a no-op
(defmethod emit-latex (stream thing &key newline)
  (declare (ignore newline)))

(defmethod emit-latex (stream (thing string) &key newline)
  (declare (ignore newline))
  (format stream "~A" thing))

(defmethod emit-latex (stream (thing string) &key newline)
  (format stream "~A~:[~;~%~]" thing newline))

(defmethod emit-latex (stream (thing character) &key newline)
  (emit-latex stream (string thing) :newline newline))

(defmethod emit-latex (stream (thing cons) &key (newline nil newline-supplied-p))
  (cond ((atom (car thing))
         (apply #'emit-latex-gf stream (car thing) (cdr thing)
                (when newline-supplied-p `(:newline ,newline))))
        ((listp (car thing))
         (apply #'emit-latex-gf stream (caar thing) (cdr thing)
                (when newline-supplied-p `(:newline ,newline))))))
               
(defmethod emit-latex (stream (thing (eql :nbsp)) &key (newline nil))
  (emit-latex stream "~" :newline newline))

(defmethod emit-latex (stream (thing (eql :hr)) &key (newline nil))
  (declare (ignore newline))
  (emit-latex-newline stream))

(defmethod emit-latex (stream (thing (eql :newline)) &key (newline nil))
  (declare (ignore newline))
  (emit-latex stream "\\\\" :newline t))

(defmethod emit-latex (stream (thing (eql :pause)) &key (newline nil))
  (emit-latex stream "\\pause" :newline newline))

(defmethod emit-latex (stream (thing (eql :hfill)) &key (newline nil))
  (emit-latex stream "\\hfill" :newline newline))

(defmethod emit-latex (stream (thing (eql :qquad)) &key (newline nil))
  (emit-latex stream "\\qquad" :newline newline))

(defmethod emit-latex (stream (thing (eql :quad)) &key (newline nil))
  (emit-latex stream "\\quad" :newline newline))

(defun emit-latex-freshline (stream)
  (format stream "~&"))

(defun emit-latex-newline (stream)
  (format stream "~%"))

;;; default is a no-op
(defmethod emit-latex-gf (stream type children &key newline)
  (declare (ignore newline)))

(defun emit-children-to-string (children)
  (cond ((null children) nil)
        ((listp children)
         (apply #'concatenate 'string
                (loop for c in children collect (emit-latex nil c))))
        (t children)))

(defmethod emit-latex-gf (stream (type (eql :p)) children &key (newline t))
  (emit-latex-freshline stream)
  (dolist (c children)
    (emit-latex stream c))
  (when (or newline t)
    (emit-latex-newline stream)
    (emit-latex-newline stream)))

(defun emit-latex-command (stream command children
                           &key
                           (newline t)
                           (initial-freshline t)
                           (options))
  (format stream "~:[~;~&~]\\~A~@[[~A]~]~@[{~A}~]~:[~;~%~]"
          initial-freshline
          command
          options
          (cond ((null children) nil)
                ((listp children)
                 (apply #'concatenate 'string
                        (loop for c in children collect (emit-latex nil c))))
                (t children))
          newline))
  
(defun emit-latex-command-2 (stream command &key options arg1 arg2 (newline t))
  (format stream "~&\\~A~@[[~A]~]~@[{~A}~]~@[{~A}~]~:[~;~%~]" command options arg1 arg2 newline))

(defun emit-latex-command-3 (stream command section &key options arg1 arg2 (newline t))
  (format stream "~&\\~A{~A}~@[[~A]~]~@[{~A}~]~@[{~A}~]~:[~;~%~]" command section options arg1 arg2 newline))

(defun emit-latex-command-4 (stream command children
                           &key
                           (newline t)
                           (initial-freshline t)
                           (options))
  (format stream "~:[~;~&~]\\~A~@[[~A]~]{"
          initial-freshline
          command
          options)
  (loop for c in children do (emit-latex stream c))
  (format stream "}~:[~;~%~]" newline))

;;;
;;; takes options command, options, arg1, arg2 and arg3, e.g.:
;;; (emit-latex-command-5 nil "command" :options "options" :arg1 "arg1" :arg2 "arg2")
;;; produces "\\command[options]{arg1}[arg2]{arg3}"
;;;
(defun emit-latex-command-5 (stream command &key options arg1 arg2 arg3 (newline t))
  (format stream "~&\\~A~@[[~A]~]~@[{~A}~]~@[[~A]~]~@[{~A}~]~:[~;~%~]"
          command options arg1 arg2 arg3 newline))

(defun emit-latex-command-6 (stream command children
                           &key
                           (newline t)
                           (initial-freshline t)
                           (arg))
  (format stream "~:[~;~&~]\\~A~@[{~A}~]~:[~;{~]"
          initial-freshline
          command
          arg
          children)
  (loop for c in children do (emit-latex stream c))
  (format stream "~:[~;}~]~:[~;~%~]" children newline))

(defun emit-latex-parameter (stream command children &key (newline t))
  (format stream "~&\\~A~@[ ~A~]~:[~;~%~]"
          command
          (cond ((null children) nil)
                ((listp children)
                 (apply #'concatenate 'string
                        (loop for c in children collect (emit-latex nil c))))
                (t children))
          newline))

(defmethod emit-latex-gf (stream (type (eql :div)) children &key newline)
  (loop for c in children collect
       (emit-latex stream c))
  (when newline (emit-latex-newline stream)))

(defmethod emit-latex-gf (stream (type (eql :span)) children &key newline)
  #+nil (format stream "~{~A~}~:[~;~%~]"
                (loop for c in children collect (emit-latex nil c))
                newline)
  (loop for c in children collect (emit-latex stream c)))

(defun emit-latex-block (command stream children &key newline)
  (format stream "{\\~A ~{~A~}}~:[~;~%~]" command
          (loop for c in children collect (emit-latex nil c))
          newline))

(defmacro with-latex-block (command stream &rest rest)
  `(progn
     (format ,stream "{\\~A " ,command)
     ,@rest
     (format ,stream "}~:[~;~%~]" t)))

(defmethod emit-latex-gf (stream (type (eql :i)) children &key newline)
  (emit-latex-block "it" stream children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :b)) children &key newline)
  (emit-latex-block "bf" stream children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :sc)) children &key newline)
  (emit-latex-block "sc" stream children :newline newline))

(defparameter *document-single-space-count* 0)

(defun single-space (stream)
  (cond ((equal *document-class* "ucthesis")
         (incf *document-single-space-count*)
         (emit-latex stream "\\ssp" :newline t))
        ((equal *document-class* "beamer"))
        ((equal *document-class* "llncs"))
        ((equal *document-class* "acm_proc_article-sp"))
        (t (emit-latex stream (format nil "\\baselineskip~A" "12pt") :newline t))))

(defun default-space (stream)
  (cond ((equal *document-class* "ucthesis")
         (unless (plusp (decf *document-single-space-count*))
           (emit-latex stream (format nil "\\dsp") :newline t)))
        ((equal *document-class* "beamer"))
        ((equal *document-class* "llncs"))
        ((equal *document-class* "acm_proc_article-sp"))
        (t (emit-latex stream (format nil "\\baselineskip~A" *baseline-skip*) :newline t))))

(defmethod emit-latex-gf (stream (type (eql :pre)) children &key (newline nil))
  (declare (ignore newline))
  (emit-latex-newline stream)
  (single-space stream)
  (emit-latex-command stream "begin" '("verbatim") :newline nil)
  (format stream "~{~A~}"
          (loop for c in children collect (emit-latex nil c)))
  (emit-latex-command stream "end" '("verbatim"))
  (default-space stream))


(defmethod emit-latex-gf (stream (type (eql :code)) children &key (newline nil))
  (format stream "~{~A~}~:[~;~%~]"
          (loop for c in children collect (emit-latex nil c))
          newline))

(defmethod emit-latex-gf (stream (type (eql :pseudocode)) children &key (newline t))
  (destructuring-bind (&key name (parameters " ") label)
      (car children)
    (single-space stream)
    (when (equal *document-class* "beamer")
      (emit-latex stream "{\\scriptsize"))
    (emit-latex-command-3 stream "begin" "pseudocode" :options "framebox" :arg1 name :arg2 parameters :newline nil)
    (format stream "~{~A~}~:[~;~%~]"
            (loop for c in (cdr children)
               collect (emit-latex nil c))
            newline)
    (when label
      (emit-latex-command stream "label" label :newline t))
    (emit-latex-command stream "end" "pseudocode" :newline newline)
    (when (equal *document-class* "beamer")
      (emit-latex stream "}" :newline t))
    (emit-latex-newline stream)
    (default-space stream)))

(defmethod emit-latex-gf (stream (type (eql :soutput)) children &key (newline))
  (declare (ignore newline))
  (single-space stream)
  (emit-latex stream "{\\scriptsize")
  (emit-latex-command stream "begin" "Soutput")
  (dolist (c children)
    (emit-latex stream c))
  (emit-latex-command stream "end" "Soutput")
  (emit-latex stream "}" :newline t)
  (default-space stream))

(defmethod emit-latex-gf (stream (type (eql :results)) children &key (newline nil))
  (format stream "~{~A~}~:[~;~%~]"
          (loop for c in children collect (emit-latex nil c))
          newline))

(defmethod emit-latex-gf (stream (type (eql :clearpage)) children &key (newline t))
  (emit-latex-command stream "clearpage" nil :newline t))

(defun get-headings ()
  (cond ((equal *document-class* "ucthesis")
         *thesis-headings*)
        (t *article-headings*)))

(defvar *default-font-size* "10pt")

(defun setup-headings ()
  (cond ((member *document-class* '("ucthesis") :test #'equal)
         (setf *document-options* '("11pt")))
        ((member *document-class* '("beamer") :test #'equal)
         (setf *document-options* '("10pt")))
        ((member *document-class* '("res") :test #'equal)
         (setf *document-options* '("margin" "line")))
        ((member *document-class* '("acm_proc_article-sp") :test #'equal)
         (setf *document-options* nil))
        ((member *document-class* '("llncs") :test #'equal)
         (setf *document-options* '("oribibl")))
        (t (setf *document-options* `(,*default-font-size*)))))

(defmethod emit-latex-gf (stream (type (eql :appendices)) children &key (newline t))
  (declare (ignore newline))
  (single-space stream)
  (emit-latex-command stream "appendix" nil)
  (loop for c in children
     do (emit-latex stream c))
  (default-space stream))

(defparameter *h1-default-clearpage* t)
(defparameter *h2-default-clearpage* t)

(defparameter *default-number-sections* t)

(defmethod emit-latex-gf (stream (type (eql :h1)) children &key (newline t))
  (ch-util::with-keyword-args ((label
                                (clearpage *h1-default-clearpage*)
                                (no-number (not *default-number-sections*)))
                               children)
      children
    (when (and clearpage (not (eql clearpage :nil)))
      (emit-latex-command stream "clearpage" nil :newline t))
    (when (equal *document-class* "ucthesis")
      (emit-latex stream "\\pagestyle{fancyplain}" :newline t)
      (emit-latex stream "\\cfoot{}" :newline t))
    (emit-latex-command stream (format nil "~A~:[~;*~]"
                                       (cdr (assoc type (get-headings)))
                                       no-number)
                        children :newline newline)
    (when label
      (emit-latex-command stream "label" label :newline newline))))


(defmethod emit-latex-header (stream type children &key (newline t))
  (ch-util::with-keyword-args ((label
                                (clearpage *h2-default-clearpage*)
                                (no-number (not *default-number-sections*))) children)
      children
    (when (and clearpage (not (eql clearpage :nil)))
      (emit-latex-command stream "clearpage" nil :newline t))
    (single-space stream)
    (emit-latex-command stream (format nil "~A~:[~;*~]"
                                       (cdr (assoc type (get-headings)))
                                       no-number)
                        children :newline newline)
    (when label
      (emit-latex-command stream "label" label :newline newline))
    (default-space stream)))

(defmethod emit-latex-gf (stream (type (eql :h2)) children &key (newline t))

  (emit-latex-header stream type children :newline newline)

  #+nil (ch-util::with-keyword-args ((label) children)
            children
          (when label
            (emit-latex-command stream 'label label :newline newline))))

(defmethod emit-latex-gf (stream (type (eql :h3)) children &key (newline t))
  #+nil (when (equal *document-class* "article")
    (emit-latex-command stream "res" '("-14pt")))

  (emit-latex-header stream type children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :h4)) children &key (newline t))
  #+nil (when (equal *document-class* "article")
          (emit-latex-command stream "res" '("-14pt")))
  (emit-latex-header stream type children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :part)) children &key (newline nil))
  (emit-latex-command stream "part" (format nil "~{~A~^, ~}" children) :newline newline))

(defmethod emit-latex-gf (stream (type (eql :bibcite)) children &key (newline nil))
  (emit-latex-command stream "cite" (format nil "~{~A~^, ~}" children) :initial-freshline nil :newline newline))

(defmethod emit-latex-gf (stream (type (eql :caption)) children &key (newline nil))
  (ch-util::with-keyword-args (((figure-label t)) children)
      children
    (let ((label (if figure-label
                     "caption"
                     "caption*")))
      (emit-latex-command stream label children :newline newline :initial-freshline nil))))

(defmethod emit-latex-gf (stream (type (eql :label)) children &key (newline nil))
  (emit-latex-command stream "label" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :ref)) children &key newline)
  (declare (ignore newline))
  (emit-latex-command stream "ref" children :initial-freshline nil :newline nil))

(defmethod emit-latex-gf (stream (type (eql :nbsp)) children &key newline)
  (declare (ignore newline children))
  (emit-latex stream "~"))

(defmethod emit-latex-gf (stream (type (eql :centering)) children &key (newline nil))
  (emit-latex-command-6 stream "centering" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :center2)) children &key (newline nil))
  (declare (optimize (debug 3))
           (ignore newline))
  (emit-latex-newline stream)
  (emit-latex-command stream "begin" '("centering") :newline nil)
  (dolist (p children)
    (emit-latex stream p :newline nil))
  (emit-latex-command stream "end" '("centering")))

(defparameter *image-copy-path* nil)

(defparameter *copy-image-files* nil)

(defmethod emit-latex-gf (stream (type (eql :image)) children &key (newline t))
  (destructuring-bind (image-pathname &key
                                      (width)
                                      (copy *copy-image-files*)
                                      (convert-png-to-eps nil)
                                      &allow-other-keys)
      children
    (let ((image-file (ch-util:unix-name image-pathname)))
      (when (and copy *image-copy-path*)
        (let ((new-file (merge-pathnames (make-pathname :name (pathname-name image-file)
                                                        :type (pathname-type image-file)
                                                        :directory (cons :relative (nthcdr 5 (pathname-directory image-file))))
                                          *image-copy-path*)))
          (ensure-directories-exist new-file)
          (cond ((and convert-png-to-eps
                      (equal (pathname-type image-file)
                             "png"))
                 (setf new-file (merge-pathnames (make-pathname :type "eps")
                                                 new-file))
                 (print (sb-ext::run-program "/Users/sly/bin/png2eps"
                                             (list image-file)
                                             :environment '("PATH=/bobo/bin:/sw/bin:/sw/sbin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/Users/sly/bin:/opt/local/bin:")
                                             :if-output-exists :supersede
                                             :output new-file)))
                (t
                 (cl-fad::copy-file image-file new-file :overwrite t)))
          (print (cons
                  new-file
                  image-file))
          (setf image-file new-file)))
      (apply #'emit-latex-command-2
             stream
             "includegraphics"
             (list*
              :arg1 image-file
              :newline newline
              (when width `(:options ,(format nil "width=~A" width))))))))

(defparameter *default-figure-placement* "tbp")

(defmethod emit-latex-gf (stream
                          (type (eql :figure))
                          children
                          &key
                          (newline t)
                          (placement *default-figure-placement*))
  (declare (ignorable newline))
  (ch-util::with-keyword-args (((placement placement)
                                (increment-counter t)
                                (subfigure-start 0)
                                label) children)
      children
    (emit-latex-command-3 stream "begin" "figure" :options placement :newline nil)
    (when subfigure-start
      (let ((subfigure-start (if (numberp subfigure-start)
                                 subfigure-start
                                 (parse-integer subfigure-start))))
        (when (plusp subfigure-start)
          (list (emit-latex-command-2 stream "addtocounter"
                                      :arg1 "subfigure"
                                      :arg2 subfigure-start
                                      :newline nil)))))
    (dolist (p children)
      (emit-latex stream p :newline nil))
    (when label
      (emit-latex-command stream "label" label :newline t))
    (emit-latex-command stream "end" "figure")
    (when (or (not increment-counter)
              (eql increment-counter :nil))
      (list (emit-latex-command-2 stream "addtocounter" :arg1 "figure" :arg2 "-1" :newline nil)))))

(defmethod emit-latex-gf (stream
                          (type (eql :figure*))
                          children
                          &key
                          (newline t)
                          (placement *default-figure-placement*))
  (declare (ignorable newline))
  (ch-util::with-keyword-args (((placement placement) label) children)
      children
    (emit-latex-command-3 stream "begin" "figure*" :options placement :newline nil)
    (dolist (p children)
      (emit-latex stream p :newline nil))
    (when label
      (emit-latex-command stream "label" label :newline t))
    (emit-latex-command stream "end" "figure*")))

(defmethod emit-latex-gf (stream (type (eql :subfigure)) children &key (newline nil))
  (ch-util::with-keyword-args ((caption (increment-counter t)) children)
      children
    (when caption
      (setf caption (emit-children-to-string caption)))
    (apply #'concatenate 'string
           (apply #'emit-latex-command
                  stream "subfigure"
                  children
                  (append
                   (when caption `(:options ,caption))
                   `(:newline ,newline)))
           (unless (or caption increment-counter)
             (list (emit-latex-command-2 stream "addtocounter" :arg1 "subfigure" :arg2 "-1" :newline nil))))))

(defmethod emit-latex-gf (stream (type (eql :document-element)) children &key (newline t))
  (destructuring-bind (element &rest rest) children
    (emit-latex-command-3 stream "begin" element :newline newline)
    (dolist (p rest)
      (emit-latex stream p :newline nil))
    (when (string-equal element "abstract")
      (cond ((equal *document-class* "ucthesis")
             (emit-latex-command stream "abstractsignature" nil))))
    (emit-latex-command stream "end" element)))

(defmethod emit-latex-gf (stream (type (eql :document-command)) children &key (newline t))
  (destructuring-bind (element &rest rest) children
    (emit-latex-command-6 stream element rest :newline newline)))

;;; "scicite" "pslatex" "times" "epsfig" "graphs" "newcent"
   
(defun include-contents-of-file-file (stream file)
  (emit-latex stream (ch-util::contents-of-file file)))

(defun latex-document-format (stream)
  (dolist (package *latex-packages*)
    (if (consp package)
        (emit-latex-command-2 stream "usepackage" :options (cdr package) :arg1 (car package))
        (emit-latex-command stream "usepackage" package)))
  (loop for (param . val) in *document-format-parameters*
     do (emit-latex-parameter stream param val))
  (dolist (command *document-latex-commands*)
    (emit-latex stream command)))

(defun latex-document (stream sexp &key (options *document-options*) (class *document-class*))
  (emit-latex-command-2 stream "documentclass"
                        :options (format nil "~{~A~^,~}" options) :arg1 class :newline t)
  (unless (equal *document-class* "llncs")
    (emit-latex-command-2 stream "setcounter"
                          :arg1 "secnumdepth"
                          :arg2 *section-numbering-depth* :newline t))
  (latex-document-format stream)

  (when *document-title*
    (emit-latex-command stream "title" (list *document-title*)))
  (when *document-titlerunning*
    (emit-latex-command stream "titlerunning" (list *document-titlerunning*)))
  (when (and *document-subtitle*
             (equal *document-class* "beamer"))
    (emit-latex-command stream "subtitle" (list *document-subtitle*)))
  (when *document-author*
    (emit-latex-command stream "author" (list *document-author*)))
  (when *document-tocauthor*
    (emit-latex-command stream "tocauthor" (list *document-tocauthor*)))
  (when *document-authorrunning*
    (emit-latex-command stream "authorrunning" (list *document-authorrunning*)))
  (when *document-address*
    (emit-latex-command stream "address" (list *document-address*)))
  (when *document-institute*
    (emit-latex-command stream "institute" (list *document-institute*)))
  (when *document-date*
    (emit-latex-command stream "date" (list *document-date*)))
  (when (equal *document-class* "ucthesis")
    (when *document-degree-year*
      (emit-latex-command stream "degreeyear" (list *document-degree-year*)))
    (when *document-degree-semester*
      (emit-latex-command stream "degreesemester" (list *document-degree-semester*)))
    (when *document-degree*
      (emit-latex-command stream "degree" (list *document-degree*)))
    (when *document-chair*
      (emit-latex-command stream "chair" (list *document-chair*)))
    (when *document-other-members*
      (emit-latex-command stream "othermembers" (list *document-other-members*)))
    (when *document-number-of-members*
      (emit-latex-command stream "numberofmembers" (list *document-number-of-members*)))
    (when *document-prev-degrees*
      (emit-latex-command stream "prevdegrees" (list *document-prev-degrees*)))
    (when *document-field*
      (emit-latex-command stream "field" (list *document-field*)))
    (when *document-campus*
      (emit-latex-command stream "campus" (list *document-campus*))))
  
  (cond ((equal *document-class* "ucthesis")
         (emit-latex stream
                     (format nil
                             "\\def\\dsp{\\def\\baselinestretch{~A}\\large\\normalsize}"
                             *baseline-stretch*)
                     :newline t)
         (emit-latex stream "\\dsp" :newline t)
        
         (emit-latex stream "\\addtolength{\\headheight}{\\baselineskip}" :newline t)
         #+nil
         (progn
           (emit-latex stream "\\lhead[\\fancyplain{}\\sl\\thepage]{\\fancyplain{}\\sl\\rightmark}" :newline t)
           (emit-latex stream "\\rhead[\\fancyplain{}\\sl\\leftmark]{\\fancyplain{}\\sl\\thepage}" :newline t)
           (emit-latex stream "\\lhead[\\fancyplain{}\\bfseries\\thepage]{\\fancyplain{}\\bfseries\\rightmark}" :newline t)
           (emit-latex stream "\\rhead[\\fancyplain{}\\bfseries\\leftmark]{\\fancyplain{}\\bfseries\\thepage}" :newline t))
         (progn (emit-latex stream "\\lhead[\\fancyplain{}{}]{\\fancyplain{}{\\bfseries\\leftmark}}" :newline t)
                (emit-latex stream "\\rhead[\\fancyplain{}{}]{\\fancyplain{\\bfseries\\thepage}{\\bfseries\\thepage}}" :newline t))
         (emit-latex stream "\\hyphenpenalty=1000" :newline t)
         (emit-latex stream "\\clubpenalty=500" :newline t)
         (emit-latex stream "\\widowpenalty=500" :newline t)
         (princ *thesis-preamble* stream))
        ((equal *document-class* "beamer")
         (princ *beamer-preamble* stream))
        ((equal *document-class* "res")
         (princ *res-preamble* stream))
        ((equal *document-class* "llncs")
         (princ *llncs-preamble* stream))
        ((equal *document-class* "acm_proc_article-sp")
         (princ *acm-proc-article-preamble* stream))
        (t
         (emit-latex stream "\\geometry{verbose,tmargin=1in,bmargin=1in,lmargin=1in,rmargin=1in}" :newline t)
         (princ *article-preamble* stream)))
  
  (emit-latex-command stream "begin" "document")
  (emit-latex-freshline stream)
  
  (cond ((equal *document-class* "beamer"))
        ((equal *document-class* "res"))
        ((equal *document-class* "acm_proc_article-sp"))
        (t
         (when *document-titlepage*
           (emit-latex stream "\\maketitle" :newline t))))
  
  (cond ((equal *document-class* "llncs"))
        ((equal *document-class* "acm_proc_article-sp"))
        (t
         (emit-latex stream "\\let\\mypdfximage\\pdfximage" :newline t)
         (emit-latex stream "\\def\\pdfximage{\\immediate\\mypdfximage}" :newline t)))
  
  (cond ((equal *document-class* "ucthesis")
         (progn
           (when *thesis-approval-page*
             (emit-latex stream "\\approvalpage" :newline t))
           (emit-latex stream "\\copyrightpage" :newline t)))
        ((equal *document-class* "beamer"))
        ((equal *document-class* "llncs"))
        ((equal *document-class* "acm_proc_article-sp"))
        (t
         (emit-latex stream (format nil "\\baselineskip~A" *baseline-skip*) :newline t)))
  
  (dolist (p sexp)
    (emit-latex stream p))

  (emit-latex-command stream "end" "document"))

(defun latex-use-package (stream package)
  (emit-latex-command stream "usepackage" package))

(defun latex-use-packages (stream &rest packages)
  (mapcar #'(lambda (x) (latex-use-package stream x)) packages))

;;; Bibliography stuff

(defun latex-cite (stream ref)
  (emit-latex-command stream "cite" ref))

(defun latex-bibliography-style (stream style)
  (emit-latex-command stream "bibliographystyle" style))

(defun latex-bibliography (stream bib)
  (emit-latex-command stream "bibliography" bib))

;;; The main entry point to this stuff

(defmethod render-as ((type (eql :latex)) sexp file)
  (let ((*document-class* *document-class*)
        (*document-degree-year* *document-degree-year*)
        (*document-degree-semester* *document-degree-semester*)
        (*document-degree* *document-degree*)
        (*document-chair* *document-chair*)
        (*document-other-members* *document-other-members*)
        (*document-number-of-members* *document-number-of-members*)
        (*document-prev-degrees* *document-prev-degrees*)
        (*document-field* *document-field*)
        (*document-campus* *document-campus*)
        (*latex-packages* *latex-packages*)
        (*document-format-parameters* *document-format-parameters*))
    (setup-headings))
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (latex-document stream sexp)))


;; the intent here is that :bibliography is a tag with no children and
;; that we should output the appropriate latex bibliography here.
(defmethod emit-latex-gf (stream (type (eql :bibliography)) children &key (newline t))
  (declare (optimize (debug 3)) (ignore newline))
  (destructuring-bind (&rest rest &key (clearpage t) &allow-other-keys)
      children
    (declare (ignore rest))
    (when (and clearpage (not (eql clearpage :nil)))
      (emit-latex-command stream "clearpage" nil :newline t)))
  (unless (member *document-class* '("beamer" "llncs") :test 'equal)
    (emit-latex stream (format nil "\\baselineskip~A" "11pt") :newline t))
  (let ((style-function (bibtex-compiler:find-bibtex-style *bibtex-style*))
      (bibtex-runtime:*cite-keys* (reverse *cite-keys*))
      (bibtex-runtime:*bib-macros* *bibtex-macros*)
      (bibtex-runtime:*bib-database* *bibtex-database*)
      (bibtex-runtime:*bib-files* nil)
      (bibtex-runtime:*bbl-output* stream))
  (funcall style-function)))

(defmethod emit-latex-gf (stream (type (eql :table-of-contents)) children &key (newline t))
  (declare (ignore newline))
  (emit-latex stream "\\tableofcontents" :newline t))

(defmethod emit-latex-gf (stream (type (eql :list-of-figures)) children &key (newline t))
  (declare (ignore newline))
  (emit-latex stream "\\listoffigures" :newline t))

(defmethod emit-latex-gf (stream (type (eql :table-of-tables)) children &key (newline t))
  (declare (ignore newline))
  (emit-latex stream "\\listoftables" :newline t))


;;; equations

;; FIXME!!!

(defmethod emit-latex-gf (stream (type (eql :quotation)) children &key (newline t))
  (declare (ignorable newline))
  (emit-latex-command-3 stream "begin" "quotation" :newline nil)
  (dolist (p children)
    (emit-latex stream p :newline nil))
  (emit-latex-command stream "end" "quotation" :newline t))

(defmethod emit-latex-gf (stream (type (eql :equation)) children &key (newline t))
  (declare (ignorable newline))
  (emit-latex-command-3 stream "begin" "equation" :newline nil)
  (dolist (p children)
    (emit-latex stream p :newline nil))
  (emit-latex-command stream "end" "equation" :newline t))

(defmethod emit-latex-gf (stream (type (eql :matrix)) children &key (newline t))
  (declare (ignorable newline))
  (princ "$" stream)
  (emit-latex-command-3 stream "begin" "matrix" :newline nil)
  (dolist (p children)
    (emit-latex stream p :newline nil))
  (emit-latex-command stream "end" "matrix" :newline nil)
  (princ "$" stream))

(defmethod emit-latex-gf (stream (type (eql :bmatrix)) children &key (newline t))
  (declare (ignorable newline))
  (princ "$" stream)
  (emit-latex-command-3 stream "begin" "bmatrix" :newline nil)
  (dolist (p children)
    (emit-latex stream p :newline nil))
  (emit-latex-command stream "end" "bmatrix" :newline nil)
  (princ "$" stream))

;;; tables

(defmethod emit-latex-gf (stream (type (eql :table)) children &key newline)
  (destructuring-bind ((&key cols
                             top-line) (&rest children))
      (apply #'ch-util::remove-keywordish-args '(:cols
                                                 :top-line) children)
    (emit-latex-command-3 stream "begin" "tabular" :arg1 cols)
    (when top-line
      (emit-latex stream (format nil "\\hline~%")))
    (loop for c in children collect
         (emit-latex stream c))
    (emit-latex-command-3 stream "end" "tabular")
    (when newline (emit-latex-newline stream))))

(defun emit-table-row (stream children &key (newline t))
  (emit-latex stream (format nil "~{~A~^ & ~}\\\\"
                             (mapcar #'(lambda (x)
                                         (emit-latex nil x))
                                     children)) :newline newline))

(defmethod emit-latex-gf (stream (type (eql :longtable)) children &key newline)
  (destructuring-bind ((&key cols
                             top-line
                             heading
                             caption
                             (first-heading heading)
                             (first-caption caption)
                             (font-size "small")) (&rest children))
      (apply #'ch-util::remove-keywordish-args '(:cols
                                                 :top-line
                                                 :heading
                                                 :caption
                                                 :first-heading
                                                 :first-caption
                                                 :font-size) children)
    
    (single-space stream)
    (with-latex-block font-size
      stream
      (emit-latex-command-3 stream "begin" "longtable" :arg1 cols)

      (when first-caption
        (destructuring-bind (first-caption)
            first-caption
          (emit-latex-command stream "caption" first-caption :newline nil)
          (emit-latex stream "\\\\" :newline t)))

      (when top-line
        (emit-latex stream "\\hline" :newline t))

      (when first-heading
        (emit-table-row stream (car first-heading))
        (loop for c in (cdr first-heading)
           do (emit-latex stream c)))
      (emit-latex stream "\\endfirsthead" :newline t)

      (when caption
        (destructuring-bind (caption)
            caption
          (emit-latex-command stream "caption" caption :newline nil)
          (emit-latex stream "\\\\" :newline t)))

      (when top-line
        (emit-latex stream "\\hline" :newline t))

      (when heading
        (emit-table-row stream (car heading))
        (loop for c in (cdr heading)
           do (emit-latex stream c)))
      (emit-latex stream "\\endhead" :newline t)

      (loop for c in children collect
           (emit-latex stream c))
      (emit-latex-command-3 stream "end" "longtable")
      (when newline (emit-latex-newline stream)))
    (default-space stream)))

(defmethod emit-latex-gf (stream (type (eql :table-row)) children &key (newline t))
  (destructuring-bind ((&key multicolumn (spec "|c|")) (&rest children))
      (apply #'ch-util::remove-keywordish-args '(:multicolumn :spec) children)
    (if multicolumn
        (progn (apply #'emit-latex-command-3 stream "multicolumn" multicolumn
                      :arg1 spec
                      :newline nil
                      :arg2 (mapcar #'(lambda (x)
                                        (emit-latex nil x))
                                    children))
               (emit-latex stream "\\\\" :newline newline))
        (emit-latex stream (format nil "~{~A~^ & ~}\\\\"
                                   (mapcar #'(lambda (x)
                                               (emit-latex nil x))
                                           children)) :newline newline))))

(defmethod emit-latex-gf (stream (type (eql :horizontal-line)) children &key (newline t))
  (emit-latex stream "\\hline" :newline newline))

;;;
;;;
;;; beamer stuff
(defmethod emit-latex-gf (stream (type (eql :slide)) children &key (newline t))
  (ch-util::with-keyword-args ((slide-title) children)
      children
    (emit-latex-command-2 stream "frame")
    (format stream "{")
    (when slide-title (emit-latex-command-2 stream "frametitle" :arg1 slide-title))
    (loop for c in children do (emit-latex stream c))
    (format stream "}")
    (when newline
      (emit-latex-newline stream))))

(defmethod emit-latex-gf (stream (type (eql :title-page)) children &key (newline t))
  (declare (ignore newline))
  (emit-latex-command-2 stream "titlepage"))

(defmethod emit-latex-gf (stream (type (eql :table-of-contents)) children &key (newline t))
  (declare (ignore newline))
  (emit-latex-command-2 stream "tableofcontents"))

(defmethod emit-latex-gf (stream (type (eql :list)) children &key (newline t))
  (emit-latex-command stream "begin" '("itemize") :newline newline)
  (loop for c in children do (emit-latex stream c))
  (emit-latex-command stream "end" '("itemize") :newline newline))

(defmethod emit-latex-gf (stream (type (eql :item)) children &key (newline t))
  (declare (ignore newline))
  (emit-latex-command-2 stream "item " :newline nil)
  (loop for c in children do (emit-latex stream c)))

(defmethod emit-latex-gf (stream (type (eql :colorbox)) children &key (newline nil))
  (ch-util::with-keyword-args (((color "white")) children)
      children
    (emit-latex-command-6 stream "colorbox" children :arg color :newline newline)))

;;; columns for beamer
(defmethod emit-latex-gf (stream (type (eql :columns)) children &key (newline t))
  (ch-util::with-keyword-args ((format) children)
      children
    (emit-latex-command-5 stream "begin" :arg1 "columns" :arg2 format :newline newline)
    (loop for c in children do (emit-latex stream c))
    (emit-latex-command stream "end" '("columns") :newline newline)))

(defmethod emit-latex-gf (stream (type (eql :column)) children &key (newline t))
  (ch-util::with-keyword-args ((width) children)
      children
    (emit-latex-command-2 stream "begin" :arg1 "column" :arg2 (format nil "~A\\textwidth" width) :newline newline)
    (loop for c in children do (emit-latex stream c))
    (emit-latex-command stream "end" '("column") :newline newline)))

;;; minipage (for use with captions)


(defmethod emit-latex-gf (stream (type (eql :minipage)) children &key (newline t))
  (ch-util::with-keyword-args (((width "0.5")) children)
      children
    (emit-latex-command-5 stream "begin" :arg1 "minipage" :arg2 "t"
                          :arg3 (format nil "~A\\linewidth" width) :newline newline)
    (loop for c in children do (emit-latex stream c))
    (emit-latex-command stream "end" '("minipage") :newline newline)))


;;; resume stuff

(defmethod emit-latex-gf (stream (type (eql :name)) children &key (newline nil))
  (emit-latex-command stream "name" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :employer)) children &key (newline nil))
  (emit-latex-command stream "employer" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :position-title)) children &key (newline nil))
  (emit-latex-command stream "title" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :dates)) children &key (newline nil))
  (emit-latex-command stream "dates" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :location)) children &key (newline nil))
  (emit-latex-command stream "location" children :newline newline))

(defmethod emit-latex-gf (stream (type (eql :position)) children &key (newline t))
  (emit-latex-command stream "begin" '("position") :newline newline)
  (loop for c in children do (emit-latex stream c))
  (emit-latex-command stream "end" '("position") :newline newline))

(defmethod emit-latex-gf (stream (type (eql :resume)) children &key (newline t))
  (emit-latex-command stream "begin" '("resume") :newline newline)
  (loop for c in children do (emit-latex stream c))
  (emit-latex-command stream "end" '("resume") :newline newline))

(defmethod emit-latex-gf (stream (type (eql :list1)) children &key (newline t))
  (emit-latex-command stream "begin" '("list1") :newline newline)
  (loop for c in children do (emit-latex stream c))
  (emit-latex-command stream "end" '("list1") :newline newline))

(defmethod emit-latex-gf (stream (type (eql :list2)) children &key (newline t))
  (emit-latex-command stream "begin" '("list2") :newline newline)
  (loop for c in children do (emit-latex stream c))
  (emit-latex-command stream "end" '("list2") :newline newline))

