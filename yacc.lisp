(in-package :c-parse)

;;;;process the yacc.txt file
(defparameter *yacc-txt-path*
  (merge-pathnames "yacc.txt" *path*))
(deflazy *yacc-txt* ()
  (alexandria:read-file-into-string *yacc-txt-path*))
(deflazy *yacc-txt2* (*yacc-txt*)
  (file-lines-no-whitespace-lines 
   *yacc-txt*))
(defun split-yacc (&optional (yacc (getfnc '*yacc-txt2*)))
  (multiple-value-bind (first second) 
    (values (subseq yacc 0 first)
	    (let ((value (subseq yacc (+ 1 first) second))
		  acc)
	      ;;intersperse newlines again and concatenate for esrap-liquid
	      (dolist (val value)
		(push val acc)
		(push
		 '(#\newline)
		 acc))
	      (%concatenate-string (nreverse acc))))))
(deflazy %%-positions ((yacc *yacc-txt2*))
  (multiple-value-list (%%-positions yacc)))
(deflazy *yacc-tokens-lines* (%%-positions (yacc *yacc-txt2*))
  (destructuring-bind (first second) %%-positions
    (declare (ignore second))
    (subseq yacc 0 first)))
(deflazy *yacc-definitions* (%%-positions (yacc *yacc-txt2*))
  (destructuring-bind (first second) %%-positions
    (let ((value (subseq yacc (+ 1 first) second))
	  acc)
      ;;intersperse newlines again and concatenate for esrap-liquid
      (dolist (val value)
	(push val acc)
	(push
	 '(#\newline)
	 acc))
      (%concatenate-string (nreverse acc)))))
;;;;


(define-c-parse-rule yacc-token-line ()
  (v "%token")
  (times (progn-v whitespace
		  lex-token-string)))
(define-c-parse-rule yacc-start-line ()
  (v "%start")
  (progn-v whitespace
	   lex-token-string))

;;  "return (values #<list-of-tokens> start)"
(deflazy *yacc-token-strings* (*yacc-tokens-lines*)
  (alexandria:flatten
   (mapcar (lambda (line)
	     (parse-with-garbage 'yacc-token-line line))
	   *yacc-tokens-lines*)))
(deflazy *yacc-start-string* (*yacc-tokens-lines*)
  (first
   (alexandria:flatten
    (mapcar (lambda (line)
	      (parse-with-garbage 'yacc-start-line line))
	    *yacc-tokens-lines*))))

;;FIXME::does not handle // comments
(define-c-parse-rule yacc-whitespace-or-comment ()
  (|| whitespace
      lex-yacc-multiline-comment))
(define-c-parse-rule yacc-whitespace-or-comments ()
  (postimes yacc-whitespace-or-comment))

(define-c-parse-rule yacc-grammar-one-expansion ()
  (v yacc-whitespace-or-comments)
  (|| lex-read-char
      lex-token-string))
(define-c-parse-rule yacc-rule-aux ()
  (times yacc-grammar-one-expansion))
(define-c-parse-rule yacc-rule ()
  (list* (prog1 (v lex-token-string)
	   (v yacc-whitespace-or-comments))
	 (prog1 (list*
		 (progn
		   (v #\:)
		   (v yacc-rule-aux))
		 (times (progn-v yacc-whitespace-or-comments
				 #\|
				 yacc-rule-aux)))
	   (v yacc-whitespace-or-comments)
	   (v #\;)
	   (v #\Newline))))
(define-c-parse-rule yacc-rules ()
  (times yacc-rule))

(deflazy *yacc-grammar* (*yacc-definitions*)
  (parse-with-garbage 'yacc-rules *yacc-definitions*))

(defparameter *yacc-terminal-chars* nil) ;;because characters are terminals. FIXME:documentation
(defparameter *yacc-package*
  (let ((name "YACC-SYMBOLS"))
    (or (find-package name)
	(make-package name))))
(defun yacc-symbol (x)
  (if x
      (let ((string (etypecase x
		      (string (format nil "$~a" x))
		      (character
		       (pushnew x *yacc-terminal-chars*)
		       (format nil "C~a" x))
		      (symbol (format nil "S~a" x)))))
	(intern string *yacc-package*))
      x))

(utility:eval-always
  (progn
    (struct-to-clos:struct->class
     ;;represent a contiguous region of memory
     (defstruct character-section
       data
       start
       end))
    (defun print-character-section (stream object)
      (write (character-section-data object) :stream stream)
      (format stream "<~a,~a>"
	      (character-section-start object)
	      (character-section-end object)))
    (set-pprint-dispatch 'character-section 'print-character-section)))
(defmethod equalp? ((a character-section) (b character-section))
  (and (equalp? (character-section-data a) (character-section-data b))
       (equalp? (character-section-start a) (character-section-start b))
       (equalp? (character-section-end a) (character-section-end b))))
#+nil
"A  parser  consumes  the  output  of  a  lexer,  that  produces  a  stream  of  terminals.   CL-Yacc
expects the lexer to be a function of no arguments (a
thunk
) that returns two values:  the next
terminal symbol, and the value of the symbol, which will be passed to the action associated with
a production.  At the end of the input, the lexer should return
nil
."
(defun correct-token-if-identifier (token-type string-thing)
  (let ((string (stringy string-thing)))
    (values 
     (if (and
	  ;;was marked as an IDENTIFIER
	  (symbol= token-type (load-time-value (yacc-symbol "IDENTIFIER")))
	  ;;and was found to be a typedef
	  (member string *typedef-env* :test 'string=))
	 (load-time-value (yacc-symbol "TYPEDEF_NAME"))
	 token-type)
     string)))
(defun lex-for-cl-yacc (string &key (start 0) (end nil))
  ;;cl-yacc accepts a function that returns two values. the token type and the value
  ;;when there are no more tokens, it should return (values nil nil) [?FIXME::document?]
  (lambda ()
    (block out
      (tagbody try-again
	 (multiple-value-bind (result len)
	     (parse-with-garbage 'lexer-foo string :start start)
	   (let ((old-pos start)
		 (new-pos (+ start len)))
	     (setf start new-pos)
	     (when (or
		    ;;went too far, farther than end
		    (and end (> new-pos end))
		    ;;failed to lex. length 0 means no solution for lexing was found
		    (zerop len))
	       (return-from out (values nil nil)))
	     (destructuring-bind (string-thing ignorable yacc-token-type) result
	       (declare (ignorable string-thing yacc-token-type ignorable))
	       ;;(write-char (char-code-object yacc-token-type) stream)
	       ;;(princ (stringy (car result)) stream)

	       ;;to skip over whitespace
	       (when (not yacc-token-type)
		 (go try-again))
	       
	       (multiple-value-bind (token string)
		   (correct-token-if-identifier yacc-token-type string-thing)
		 (return-from out
		   (values token
			   (make-character-section
			    :data string
			    :start old-pos
			    :end new-pos)
			   ))))))))))
(deflazy *yacc-start-symbol* (*yacc-start-string*)
  (yacc-symbol *yacc-start-string*))

(defun aux-fun234 (n)
  ;;;add a closure at the end of the grammar which lists the dump name and
  ;;;choice count
  (let ((name (first n))
	(count 0))
    ;;FIXME::keyword parameter strings are not upcase in the specification. may change?
    (let ((dump-name (utility:keywordify (string-upcase name))))
      (list* name
	     (let ((acc nil))
	       (dolist (item (rest n))
		 (push
		  (append item
			  `((lambda (&rest rest)
			      (list* ,dump-name ,count rest))))
		  acc)
		 (incf count))
	       (nreverse acc))))))
;;to output with grammar rule and form number from grammar
(deflazy *yacc-grammar-info* (*yacc-grammar*)
  (mapcar 'aux-fun234 *yacc-grammar*))

(defun tree-map (fn tree &key (max-depth -1))
  "replace each list-element in tree with (funcall fn list-element)"
  ;;(tree-map (lambda (x) (* x x)) '(1 2 (2 3) . foo)) -> (1 4 (4 9) . FOO)
  ;;FIXME:: max-depth is a hack to prevent the function from running too deep
  (labels ((rec (tree depth)
	     (cond ((atom tree) (funcall fn tree))
		   (t (cons (let ((item (first tree)))
			      (if (= depth max-depth)
				  item
				  (rec item (+ 1 depth))))
			    (let ((rest (rest tree)))
			      (if (and rest
				       (listp rest))
				  (rec rest depth)
				  rest)))))))
    (rec tree 0)))
(deflazy *yacc-grammar-symbols* (*yacc-grammar-info*)
  (tree-map (lambda (x)
	      (if (functionp x)
		  x
		  (yacc-symbol x)))
	    *yacc-grammar-info*
	    :max-depth 3))
(deflazy *yacc-token-symbols* (*yacc-token-strings*)
  (tree-map 'yacc-symbol
	    (append *yacc-token-strings*
		    *yacc-terminal-chars*)))

(defparameter *c* nil)
(defun gen-parser-code ()
  `(define-parser *c*
     (:start-symbol
      ;;don't 
      ;;,*yacc-start-symbol*
      ,(yacc-symbol "external_declaration")
      ;;we don't parse the whole thing, we can't just parse the whoe translation_unit
      ;;because of typedef declarations
      )
     (:terminals ,(getfnc '*yacc-token-symbols*))
     ,@(getfnc '*yacc-grammar-symbols*)))
(defun parse-external-declaration-unknown-length
    (lex-fun &key (start 0) (end most-positive-fixnum ;;FIXME::nonportable?
				 ;;(length string)
				 ))
  ;;lexfun is a function that lexes from start to end
  ;;cl-yacc will error if there are more tokens coming in after an external-declaration
  ;;so we catch the error, inspect what it was looking at, that should be the end
  ;;of the external-declaration
  (block out
    (tagbody try-again
       (handler-case
	   (progn	     
	     (return-from out
	       (if (= start end)
		   (values nil end)
		   (values (yacc:parse-with-lexer (funcall lex-fun start end) *c*)
			   end))))
	 (yacc-parse-error (c)
	   (when (eq nil (first (yacc-parse-error-expected-terminals c)))
	     (setf end (character-section-start (yacc-parse-error-value c)))
	     (go try-again))
	   (print (list start end))
	   (error c)
	   #+nil
	   (print "what???"))))))

(defparameter *typedef-env* nil)
(defparameter *parsed* nil)

;;;FIXME:: use incremental lengths, not absolute string positions
(defun keep-parsing (&optional (path *testpath*))
  (let ((string (alexandria:read-file-into-string (ensure-cached-no-directives path))))
    (setf *typedef-env* nil)
    (let ((start 0)
	  (end (length string))
	  (lex-fun (lambda (start end)
		     (lex-for-cl-yacc-cached :path path :string string :start start :end end)
		     #+nil
		     (lex-for-cl-yacc string :start start :end end))))
      (block exit
	(loop
	   (multiple-value-bind (cst where)
	       (parse-external-declaration-unknown-length
		lex-fun
		:start start :end end)
	     (when (and where (= where start)) ;;parsing failed
	       (return-from exit))
	     (push cst *parsed*)
	     ;;test whether the external-declaration was a typedef
	     (multiple-value-bind (value typedef-p) (cst-typedef-p cst)
	       ;;if it is
	       (when typedef-p
		 ;;then add the new names to the environment
		 (mapc
		  (lambda (x)
		    (pushnew x *typedef-env* :test 'string=))
		  value)))
	     (setf start
		   where)))))))
(defparameter *c-data*
  `("typedef struct tagNode
{
    enum tagNode* entry;

    struct tagNode* next;
} Node;
"
    "typedef enum  {SUCCESS, FAIL} (*MathFunc)(float, int), bar ,foo;"))
(defparameter *c-data-0*
  nil
  #+nil
  (mapcar 'parsefoobar *c-data*)
  )
(defun are-typedefs ()
  (mapcar 'cst-typedef *c-data-0*))

(defun cst-typedef-p (&optional (CST (alexandria:random-elt *c-data-0*)))
  "return a (values list-of-typedef'd-identifiers t) if its a typedef, (values nil nil) otherwise
depends on the CST being dumped in the form below, where node is (production-rule number &rest children)"
  (match CST
    ((list
      :EXTERNAL_DECLARATION
      1
      (list
       :DECLARATION
       1
       (list
	:DECLARATION_SPECIFIERS
	0
	(list
	 :STORAGE_CLASS_SPECIFIER
	 0
	 (character-section (data "typedef"))
	 )
	_)

       (list*
	:INIT_DECLARATOR_LIST _ names)
       (character-section (data ";"))
       ))
     ;;each typedef can define many values. this finds them
     (values
      (mapcar 'character-section-data
	      (find-$direct_declarator0$ names))
      t))))

(defun find-$direct_declarator0$ (tree)
  (let ((acc nil))
    (mapc-tree
     (lambda (node)
       (match node
	 ((list :DIRECT_DECLARATOR 0 value)
	  (push value acc))))
     tree)
    acc))

(defun mapc-tree (fun node)
  (funcall fun node)
  (when (consp node)
    (mapc-tree fun (car node))
    (mapc-tree fun (cdr node))))

(defun dump-cst (cst &optional (depth 0))
  (typecase cst
    (character-section
     (write-char #\Space)
     (write-string (character-section-data cst)))
    (otherwise
     (let ((items (cddr cst))
	   (newdepth depth))
       (unless (= 1 (list-length items))
	 (incf newdepth 1)
	 (terpri)
	 (loop :repeat depth :do (write-char #\Space)))
       (dolist (item items)
	 (dump-cst item newdepth))))))

;;;;FIXME::where to put this?
(defun start-up ()
  (eval-lexer)
  (eval (gen-parser-code))
  (values))
