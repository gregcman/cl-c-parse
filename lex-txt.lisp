(in-package :c-parse)
;;;;Process the lex.txt file
(defparameter *lex-txt-path*
  (merge-pathnames "lex.txt" *path*))
(deflazy *lex-txt* ()
  (alexandria:read-file-into-string *lex-txt-path*))
(deflazy *lex-txt2* (*lex-txt*)
  (file-lines-no-whitespace-lines 
   *lex-txt*))
;;https://docs.oracle.com/cd/E19504-01/802-5880/lex-6/index.html
;;The mandatory rules section opens with the delimiter %%.
;;If a routines section follows, another %% delimiter ends the rules section.
;;The %% delimiters must be entered at the beginning of a line, that is, without leading blanks.
;;If there is no second delimiter, the rules section is presumed to continue to the end of the program. 

;;divide the lex.txt into terminals and patterns.
;;ignore the c code for check_type and comment, instead hand-coding those
;;http://dinosaur.compilertools.net/lex/index.html <- detailed explanation of lex file format
(deflazy *lex-definitions-lines* ((lex *lex-txt2*))
  (let (;;this is where c code starts and definitions
	(first-end (position "%{" lex :test 'string=))
	;;skip over the variables at the beginning for the lex program
	(start (position-if (lambda (str)
			      (not (char= (aref str 0)
					  #\%)))
			    lex)))
    ;;terminals, called definitions
    (subseq lex start first-end)))
(deflazy *lex-rules-lines* ((lex *lex-txt2*))
  (multiple-value-bind (first-end second-end) (%%-positions lex)
    ;;patterns, called rules
    (subseq lex (+ 1 first-end) second-end)))
;;;;

(define-c-parse-rule lex-line-def ()
  (cap :def-name (v lex-token-string))
  (v whitespace)
  ;;(cap :rule (v lex-rule-start))
  (list
   (recap :def-name)
   (stringify (postimes character))))
(defun spec-lex-rule-rule (spec)
  (second spec))
(defun spec-lex-rule-name (spec)
  (first spec))

(define-c-parse-rule lex-line-rule ()
  (prog1-v lex-rule-start
	   whitespace))

;;*defs* is a list of ("name" "rule")
(defun split-lex-line-def (&optional (item "NS [a-zA-Z_]"))
  (destructuring-bind (name rule-string) (parse-with-garbage 'lex-line-def item)
    (list name (parse-with-garbage 'lex-rule-start rule-string))))

(defun split-lex-line-rule (&optional (string "asd[a-zA-Z_]fasd   {return; /* */}"))
  (multiple-value-bind (form end)
      (parse-with-garbage 'lex-line-rule string)
    ;;;FIXME::assumes that } terminates the line, which for this file does
    (let ((last-bracket (position #\} string :from-end t)))
      (list form
	    (subseq string (1+ end)
		    last-bracket)))))

;;(string-a-prefix-b-p "a" "ab") -> T
;;(string-a-prefix-b-p "ac" "ab") -> 
(defun string-a-prefix-b-p (a b)
  "test whether string a is a prefix of b"
  (when (> (length a)
	   (length b))
    ;;(error "a is longer than b")
    (return-from string-a-prefix-b-p nil)
    )
  (dotimes (index (length a))
    (unless (char= (aref a index)
		   (aref b index))
      (return-from string-a-prefix-b-p nil)))
  t)

(deflazy *processed-definitions* (*lex-definitions-lines*)
  (mapcar 'split-lex-line-def
	  *lex-definitions-lines*))
(defun pipeline (&optional (def "hello [90]"))
  (compile-to-esrap-liquid (split-lex-line-def def)))
(defun compile-to-esrap-liquid (item)
  (destructuring-bind (name rule) item
    (let ((form `(define-c-parse-rule ,(find-lex-symbol name) ()
		   ,(lex-rule-dump rule))))
      form)))
(defun load-processed-definitions ()
  `(progn
     ,@(mapcar
	'compile-to-esrap-liquid
	(getfnc '*processed-definitions*))))
(deflazy *processed-rules* (*lex-rules-lines*)
  (mapcar 'split-lex-line-rule
	  *lex-rules-lines*))
(defparameter *syms* nil)
(defun bar ()
  (let* ((processed-rules (getfnc '*processed-rules*))
	 (iota (alexandria:iota (list-length processed-rules)))
	 (syms (mapcar 'sym-name iota)))
    (setf *syms* syms)
    `(progn       
       ,@(mapcar (lambda (name x)
		   (let ((what-fun (parse-lex-def (second x))))
		     (utility:with-gensyms (parse-result)
		       `(define-c-parse-rule ,name ,()
			  (let ((,parse-result ,(lex-rule-dump (first x))))
			    (list
			     ,parse-result
			     ,what-fun
			     ,(flet ((convert-to-token (x)
				       (yacc-symbol x)))
				(case what-fun
				  (:comment
				   `(progn (v lex-comment-end)
					   ,(convert-to-token nil)))
				  (:check-type  ;;:check-type
				   `(cond
				      #+nil
				      (nil ;;FIXME::actually check for enums
				       (quote ,(convert-to-token "ENUMERATION_CONSTANT")))
				      (t
				       (quote
					,(convert-to-token "IDENTIFIER"))))
				   ;;FIXME::detect typedefs and enums
				   )
				  (otherwise `(quote ,(convert-to-token what-fun)))))))))))
		 syms
		 processed-rules)
       (define-c-parse-rule lexer-foo ()
	 ;;why? it was taking around 13 to 20 seconds to compile
	 ;;most-full-parse
	 (v reimplemented-most-full-parse *syms*)))))

(in-package :esrap-liquid)
;;change sort ->stable-sort nreverse
(defmacro esrap-liquid::most-full-parse2 (clauses)
  (once-only (clauses)
    (with-gensyms (g!-result g!-the-length g!-successful-parses
			     ;;g!-parse-errors
			     b!-max-length
			     b!-max-result
			     b!-max-cap-stash
			     b!-list-iterator)
      `(tracing-level
	 (if-debug "MOST-FULL-PARSE")
	 (multiple-value-bind (,g!-result ,g!-the-length)
	   (let (;;,g!-parse-errors
		 ,b!-max-result
		 ,g!-successful-parses
		 ,b!-max-cap-stash
		 (,b!-max-length 0))
	     (dolist (,b!-list-iterator ,clauses)
	       (the-position-boundary
		 (print-iter-state)
		 (with-saved-iter-state (the-iter)
		   (with-fresh-cap-stash
		     (handler-case (descend-with-rule ,b!-list-iterator)
		       (internal-esrap-error (e)
			 (declare (ignorable e))
			 (restore-iter-state)
			 ;;(push e ,g!-parse-errors)
			 )
		       (:no-error (res)
			 (restore-iter-state)
			 (when (> the-length ,b!-max-length)
			   (setf ,b!-max-length the-length)
			   (setf ,g!-successful-parses t)
			   (setf ,b!-max-result res)
			   (setf ,b!-max-cap-stash *cap-stash*))
			 #+nil
			 (push (list res the-length *cap-stash*)
			       ,g!-successful-parses)))))))
	     (if ,g!-successful-parses
		 (multiple-value-bind (res length stash) (values ,b!-max-result
								 ,b!-max-length
								 ,b!-max-cap-stash)
		   ,(propagate-cap-stash-upwards '*cap-stash* 'stash nil)
		   (fast-forward the-iter length)
		   (values res length))
		 (progn (if-debug "|| before failing P ~a L ~a" the-position the-length)
			(fail-parse "MOST-FULL-PARSE failed."))))
	   (if-debug "MOST-FULL-PARSE aftermath ~a ~a" the-length ,g!-the-length)
	   (incf the-length ,g!-the-length)
	   ,g!-result)))))
(in-package :c-parse)

(define-c-parse-rule reimplemented-most-full-parse (syms)
  (esrap-liquid::most-full-parse2 syms))

(defun sym-name (x)
  (find-lex-symbol (format nil "LEX-GENERATED~a" x)))

(defun eval-lexer ()
  (print "loading defs:")
  (eval (load-processed-definitions))
  (print "loading rules:")
  (eval (bar)))

;;;;
(defun parse-lex-def (text)
  (parse-with-garbage 'ad-hoc-lex-read-file text))

;;;FIXME:: fragile hack that picks out two irregular cases?
;;;or is this how to do it?
(define-c-parse-rule ad-hoc-lex-read-file ()
  (|| (progn (? whitespace)
	     (v "comment();")
	     :comment)
      (progn
	(? whitespace)
	(v "return check_type();")
	:check-type)
      lex-read-return))

(define-c-parse-rule lex-read-return ()
  (? whitespace)
  (v "return")
  (? whitespace)
  (cap :thing (|| lex-read-char
		  lex-read-token
		  lex-token-string))
  (? whitespace)
  (v #\;)
  (recap :thing))

(define-c-parse-rule lex-read-char ()
  (progm #\'
	 character
	 #\'))
(define-c-parse-rule lex-read-token ()
  (progm #\(
	 lex-token-string
	 #\)))
