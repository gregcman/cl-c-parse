(in-package :c-parse) 
;;;;implementation of the lex lexer
;;http://dinosaur.compilertools.net/lex/index.html
;;" \ [ ] ^ - ? . * + | ( ) $ / { } % < > ;;operators that need to be escaped
;;Another use of the quoting mechanism is to get a blank into an expression;
;;normally, as explained above, blanks or tabs end a rule.
;;Any blank character not contained within [] (see below) must be quoted.
;;Several normal C escapes with \ are recognized: \n is newline, \t is tab, and \b is backspace.
;;To enter \ itself, use \\. Since newline is illegal in an expression, \n must be used;
;;it is not required to escape tab and backspace. Every character but blank, tab, newline and the list above is always a text character. 

;;\ - and ^ ;;special characters for []

;;x        the character "x"
;;"x"      an "x", even if x is an operator.
;;\x       an "x", even if x is an operator.
;;[xy]     the character x or y.
;;[x-z]    the characters x, y or z.
;;[^x]     any character but x.
;;.        any character but newline.                    
;;^x       an x at the beginning of a line.                   ;;ignore
;;<y>x     an x when Lex is in start condition y.             ;;ignore
;;x$       an x at the end of a line.                         ;;ignore
;;x?       an optional x.
;;x*       0,1,2, ... instances of x.
;;x+       1,2,3, ... instances of x.
;;x|y      an x or a y.
;;(x)      an x.
;;x/y      an x but only if followed by y.                    ;;ignore
;;{xx}     the translation of xx from the definitions section.
;;x{m,n}   m through n occurrences of x

;;| repeats the lex rule to the next listed rule
(utility:eval-always
  (defparameter *lex-special-chars*
    '((#\t #\tab)
      (#\n #\Newline)
      (#\b #\backspace)
      (#\v #\vt)
      (#\f #\formfeed) ;;FIXME - see below
      (#\r #\return) ;;FIXME -> what chars are allowed?
      )))

(defun escaped-char-to-char (char)
  ;;Several normal C escapes with \ are recognized: \n is newline, \t is tab, and \b is backspace.
  (utility:etouq
    `(case char
       ,@*lex-special-chars*
       (otherwise char))))

(define-c-parse-rule lex-number ()
  (read-from-string (stringify
		     (postimes 
		      (character-ranges
		       (#\0 #\9))))))

(define-c-parse-rule lex-char-or-escaped-char ()
  (|| lex-char
      (progn (v #\\)
	     (let ((char (v character)))
	       (escaped-char-to-char char)))))
(utility:eval-always
  ;;FIXME:misnomer. not a regular expression
  (defparameter *lex-regex-operators*
    (coerce
     "\"\\[]^-?.*+|()$/{}%<>"
     'list)))

(flet ((escape (escaped-char char)
	 (if escaped-char
	     (format nil "\\~A" escaped-char)
	     (string char))))
  ;;;;different contexts have different escape seqences 
  (defun char-to-escaped-char-string (char)
    ;;used in string rule
    (let ((escaped-char 
	   (utility:etouq
	     `(case char
		,@(mapcar 'reverse *lex-special-chars*)
		(otherwise nil)))))
      (escape escaped-char char)))
  (defun char-to-escaped-char (char)
    ;;used as expression
    (let ((escaped-char 
	   (utility:etouq
	     `(case char
		,@(mapcar 'reverse *lex-special-chars*)
		(,*lex-regex-operators* char)
		(otherwise nil)))))
      (escape escaped-char char)))
  (defun char-to-escaped-char-character-class (char)
    ;;used in character class
    (let ((escaped-char
	   (utility::etouq
	     `(case char
		(#\\ #\\)
		(#\] #\])
		(#\? #\?) ;;FIXME::characters added here on a case by case basis?
		,@(mapcar 'reverse *lex-special-chars*)
		(otherwise nil)))))
      (escape escaped-char char))))

(define-c-parse-rule lex-char ()
  ;;" \ [ ] ^ - ? . * + | ( ) $ / { } % < > ;;operators that need to be escaped
  (! (utility:etouq `(|| ,@*lex-regex-operators*)))
  (v character))

(define-c-parse-rule lex-string ()
  (progm #\"
	 (stringify (utility:etouq
		      `(times (|| lex-char-or-escaped-char
				  (|| ,@(set-difference *lex-regex-operators*
							'(#\" #\\)))))))
	 #\"))

(progn
  (struct-to-clos:struct->class
   (defstruct lex-character-range
     start
     end))
  (defun print-lex-character-range (stream object)
    (format stream "~a-~a"
	    (char-to-escaped-char-character-class (lex-character-range-start object))
	    (char-to-escaped-char-character-class (lex-character-range-end object))))
  (set-pprint-dispatch 'lex-character-range 'print-lex-character-range))

(define-c-parse-rule lex-character-range ()
  ;;http://dinosaur.compilertools.net/lex/index.html
  ;;The - character indicates ranges.
  (cap :start (v lex-char-or-escaped-char))
  (v #\-)
  (cap :end (v lex-char-or-escaped-char))
  (make-lex-character-range
   :start (recap :start)
   :end (recap :end)))

(defmacro with-write-parens ((stream) &body body)
  `(prog2
       (write-char #\( ,stream)
       (progn ,@body)
     (write-char #\) ,stream)))

(progn
  (struct-to-clos:struct->class
   (defstruct lex-character-class
     negated-p
     (chars nil)))
  (defun print-lex-character-class (stream object)
    (;;with-write-parens (stream)
     progn
      (write-char #\[ stream)
      (when (lex-character-class-negated-p object)
	(write-char #\^ stream))
      (dolist (item (lex-character-class-chars object))
	(etypecase item
	  (character
	   (write-string (char-to-escaped-char-character-class item)
			 stream))
	  (lex-character-range 
	   (print-lex-character-range stream item))))
      (write-char #\] stream)))
  (set-pprint-dispatch 'lex-character-class 'print-lex-character-class))
(defun set-character-class-char (obj &rest data)
  (setf (lex-character-class-chars obj) data))

(define-c-parse-rule lex-rule-character-class ()
  ;;http://dinosaur.compilertools.net/lex/index.html
  ;;In character classes, the ^ operator must appear as the first character after the left bracket;
  ;;it indicates that the resulting string is to be complemented with respect to the computer character set. Thus
  (v #\[)
  (cap :negation (? #\^))
  (cap :chars
       ;;FIXME::what characters are allowed where?
       (utility:etouq
	 `(times (|| lex-character-range
		     lex-char-or-escaped-char
		     ,@(set-difference *lex-regex-operators*
				       '(#\]))))))
  (v #\])
  (make-lex-character-class
   :negated-p (recap :negation)
   :chars (recap :chars)))
(defparameter *print-raw* nil
  "toggle printing lex-sequence as a dot or a string. ")
(progn
  (defparameter *lex-rule-repeat-infinity* :infinity
    "signify that the rule should repeat forever")
  (struct-to-clos:struct->class
   (defstruct lex-rule-repeat
     rule
     min
     (max *lex-rule-repeat-infinity*)))
  (defun print-lex-rule-repeat (stream object)
    (;;with-write-parens (stream)
      progn
      (write (lex-rule-repeat-rule object) :stream stream)
      (let ((min (lex-rule-repeat-min object))
	    (max (lex-rule-repeat-max object)))
	(flet ((single-char (x)
		 (write-char x stream)))
	  (cond ((and
		  (not *print-raw*)
		  (eql min 0)
		  (eql max 1))
		 (single-char #\?))
		((and
		  (not *print-raw*)
		  (eql min 0)
		  (eql max *lex-rule-repeat-infinity*))
		 (single-char #\*))
		((and
		  (not *print-raw*)
		  (eql min 1)
		  (eql max *lex-rule-repeat-infinity*))
		 (single-char #\+))
		(t 
		 (format stream "{~a,~a}" min max)))))))
  (set-pprint-dispatch 'lex-rule-repeat 'print-lex-rule-repeat))

(define-c-parse-rule lex-rule-? (rule)
  (v #\?)
  (make-lex-rule-repeat
   :rule rule
   :min 0
   :max 1))
(define-c-parse-rule lex-rule-* (rule)
  (v #\*)
  (make-lex-rule-repeat
   :rule rule
   :min 0
   :max *lex-rule-repeat-infinity*))
(define-c-parse-rule lex-rule-+ (rule)
  (v #\+)
  (make-lex-rule-repeat
   :rule rule
   :min 1
   :max *lex-rule-repeat-infinity*))

(progn
  (struct-to-clos:struct->class
   (defstruct lex-rule-reference
     string))
  (defun print-lex-rule-reference (stream object)
    ;;FIXME::what characters can tokens consist of?
    (;;with-write-parens (stream)
     progn
      (format stream "{~a}"
	      (lex-rule-reference-string object))))
  (set-pprint-dispatch 'lex-rule-reference 'print-lex-rule-reference))

(define-c-parse-rule lex-rule-definition ()
  (make-lex-rule-reference
   :string
   (progm #\{
	  lex-token-string
	  #\})))
(define-c-parse-rule lex-rule-occurences (rule)
  (v #\{)
  (cap :min (v lex-number))
  (v #\,)
  (cap :max (v lex-number))
  (v #\})
  (make-lex-rule-repeat
   :rule rule
   :min (recap :min)
   :max (recap :max)))

(define-c-parse-rule white-char ()
  (|| #\Newline #\Space #\tab))
(define-c-parse-rule whitespace ()
  (postimes white-char))
(progn
  ;;FIXME::lex-rule, which handles sequences, is becoming dumping ground for
  ;;irregular lex syntax like strings and the dot ->.
  (struct-to-clos:struct->class
   (defstruct lex-rule
     data
     ;;dot
     (print-as-dot nil)
     ;;characters
     (with-parens nil)
     ;;strings and chars
     (string-print-as-char-p nil)
     string-data
     (string-p nil)))
  (defun print-lex-rule (stream object)
    ;;FIXME::what characters can tokens consist of?
    (flet ((print-stuff ()
	     (dolist (item (lex-rule-data object))
	       (format stream "~a" item))))
      (cond (;;for the . operator
	     (and (not *print-raw*)
		  (lex-rule-print-as-dot object))
	     ;;FIXME::dots are converted into lex-rule sequences.
	     ;;have separate special object for shortening?
	     (write-char #\. stream))
	    (;; for strings and characters
	     (and (not *print-raw*)
		  (lex-rule-string-p object))
	     (let ((str (lex-rule-string-data object)))
	       (cond ((and (= 1 (length str))
			   (lex-rule-string-print-as-char-p object))
		      (write-string (char-to-escaped-char (aref str 0))
				    stream))
		     (t
		      (write-char #\" stream)
		      (let ((str str))
			(dotimes (index (length str))
			  (write-string (char-to-escaped-char-string (aref str index))
					stream)))
		      (write-char #\" stream)))))
	    (;;if this was read with parentheses?
	     t
	     (if (lex-rule-with-parens object)
		 (with-write-parens (stream)
		   (print-stuff))
		 (print-stuff))))))
  (set-pprint-dispatch 'lex-rule 'print-lex-rule))
(define-c-parse-rule lex-rule-parentheses ()
  (let ((lex-rule-sequence
	 (progm #\(
		lex-rule-sequence
		#\))))
    (setf (lex-rule-with-parens lex-rule-sequence) t)
    lex-rule-sequence))


(define-c-parse-rule lex-rule-all-but-newline-rule ()
  (v #\.)
  (make-lex-rule
   :print-as-dot t
   :data
   (list
    (match-one-char
     #\Newline
     (make-lex-character-class
      :negated-p t)))))

(defun match-one-char (char &optional (character-class-rule
				       (make-lex-character-class)))
  "create a sequence rule that matches one character"
  (set-character-class-char
   character-class-rule
   char)
  (make-lex-rule-repeat
   :rule character-class-rule
   :min 1
   :max 1))
;;the string object covers both strings and individual characters
(defun match-string (string &optional (print-as-char nil))
  (make-lex-rule
   :string-data string
   :string-p t
   :string-print-as-char-p print-as-char
   :data
   (map 'list
	(lambda (char)
	  (match-one-char char))
	string)))
(define-c-parse-rule lex-rule-string ()
  (match-string (v lex-string)))
(define-c-parse-rule lex-rule-char ()
  (match-string (string (v lex-char-or-escaped-char)) t))

(progn
  (struct-to-clos:struct->class
   (defstruct lex-rule-or))
  (defun print-lex-rule-or (stream object)
    (declare (ignorable object))
    (format stream "|"))
  (set-pprint-dispatch 'lex-rule-or 'print-lex-rule-or))
(defparameter *bar-token* (make-lex-rule-or))
(define-c-parse-rule lex-rule-vertical-bar ()
  (v #\|)
  *bar-token*)

(define-c-parse-rule lex-atom (&optional (toplevel nil))
  (when toplevel
    (! whitespace))
  (let ((rule
	 (||
	  lex-rule-char
	  lex-rule-character-class
	  lex-rule-string
	  lex-rule-all-but-newline-rule
	  lex-rule-parentheses
	  lex-rule-definition)))
	;;;
    (block out
      (loop
	 (setf rule
	       (|| (v lex-rule-? rule)
		   (v lex-rule-* rule)
		   (v lex-rule-+ rule)
		   (v lex-rule-occurences rule)
		   (return-from out rule)))))))

(define-c-parse-rule lex-rule-sequence (&optional (toplevel nil))
  (make-lex-rule
   :data
   (prog1 (list* (? (v lex-atom toplevel))
		 (times
		  (||
		   lex-rule-vertical-bar
		   (v lex-atom toplevel)))))))

(define-c-parse-rule lex-rule-start ()
  (v lex-rule-sequence t))

;;character classes
;;strings <- can be replaced by a special lex-rule with all character-classes of length 1
;;numerical repetition
;;references
;;sequencing
;;options
;;all-but-newline <- not necessary? a character class?

;;"foo" -> ([f]{1,1}[o]{1,1}[o]{1,1})
;;. -> ([^\n]{1,1})

;;lex-rule-sequence sequencing -> concatenate + list-v?
;;lex-rule-or option -> ||
;;lex-rule-repeat repeat -> times
;;lex-character-class ->  [! with character] characters, || character-ranges
;;references -> references to other rules

;;;FIXME:: nasty hacks to dump esrap-liquid prettily
(defparameter *v-wrap-necessary* t)
(defmacro with-v-wrap-off (&body body)
  `(let ((*v-wrap-necessary* nil))
     ,@body))
(defmacro with-v-wrap-on (&body body)
  `(let ((*v-wrap-necessary* t))
     ,@body))
(defun lex-rule-dump-wrap (arg)
  (with-v-wrap-off
    (lex-rule-dump arg)))
(defgeneric lex-rule-dump (node))
;;sequencing
(defun divide-by-token (list token)
  ;;(divide-by-token '(1 2 3 4 5 3234 234 3 4) 3) -> ((1 2) (4 5 3234 234) (4))
  (let ((list-list ())
	(current-list))
    (flet ((save-current-list ()
	     (push (nreverse current-list) list-list)))
      (dolist (item list)
	(if (eql token item)
	    (progn (save-current-list)
		   (setf current-list nil))
	    (push item current-list)))
      (save-current-list))
    (nreverse list-list)))
(defmethod lex-rule-dump ((node lex-rule))
  ;;deletable optimization, exits prematurely
  ;;#+nil
  (flet ((exit (n)
	   (return-from lex-rule-dump n)))
    (let ((str (lex-rule-string-data node)))
      (when (lex-rule-string-p node)
	(let ((data
	       (if (lex-rule-string-print-as-char-p node)
		   (aref str 0)
		   str)))
	  (exit (if *v-wrap-necessary*
		    `(v ,data)
		    data))))))
  
  ;;each lex rule's data is a list of sub atoms and bars denoting choice.
  ;;divide by token divides the list of sub atoms by the bars
  (let ((undumped (divide-by-token (lex-rule-data node) *bar-token*)))
    (flet ((do-it ()
	     (flet ((sub-or (list)
		      
		      (let ((items
			     (mapcar 'lex-rule-dump list)))
			;;optimization, deletable
			(when (and ;;(not *v-wrap-necessary*)
			       (= 1 (length items)))
			  (return-from sub-or (first items)))  
			`(list-v ,@items))))
	       (let ((answer
		      (mapcar #'sub-or undumped)))
		 (case (length answer)
		   (1 (first answer))
		   (otherwise `(|| ,@answer)))))))
      (case (length undumped)
	(1 (with-v-wrap-on (do-it)))
	(otherwise (with-v-wrap-off (do-it)))))))
(defmethod lex-rule-dump ((node lex-rule-repeat))
  (let ((min (lex-rule-repeat-min node))
	(max (lex-rule-repeat-max node))
	(subexpr (lex-rule-dump-wrap (lex-rule-repeat-rule node))))
    ;;optimization
    (flet ((end (n)
	     (return-from lex-rule-dump n)))
      (cond
	((eql min max)
	 (case (utility:any min max)
	   (0 (end nil))
	   (1 (end subexpr)) ;;repeat exactly one time, repetition uneccessary
	   ))
	((and (eql min 0)
	      (eql max 1))
	 (end `(? ,subexpr)))
	((and (eql min 1)
	      (eql max *lex-rule-repeat-infinity*))
	 (end `(postimes ,subexpr)))))
    
    `(times ,subexpr
	    ,@(unless (zerop min)
		`(:from ,min))
	    ,@(if (eql max *lex-rule-repeat-infinity*)
		  nil
		  `(:upto ,max)))))
(defmethod lex-rule-dump ((node lex-character-class))
  (let ((chars (lex-character-class-chars node)))
    (let ((char-rules (remove-if-not 'characterp chars))
	  (range-rules
	   (mapcar (lambda (range)
		     `(,(lex-character-range-start range)
		       ,(lex-character-range-end range)))
		   (remove-if-not 'lex-character-range-p chars))))
      (let ((rules (append range-rules char-rules)))
	(let ((rules-form `(character-ranges ,@rules)))
	  (cond ((lex-character-class-negated-p node)
		 `(progn
		    (! ,rules-form)
		    (v character)))
		(t
		 ;;optimization
		 (when (and (= 1 (length char-rules))
			    (zerop (length range-rules)))
		   (let ((char (first char-rules)))
		     (return-from lex-rule-dump
		       (if *v-wrap-necessary*
			   `(v ,char)
			   char))))
		 rules-form)))))))
(defparameter *some-symbols* (make-package "LEX-C-PARSE-SYMBOLS"))
(defun find-lex-symbol (string)
  (intern string *some-symbols*))
(defmethod lex-rule-dump ((node lex-rule-reference))
  `(v ,(find-lex-symbol (lex-rule-reference-string node))))
