(in-package :c-parse)

(fiveam:in-suite* c-parse)

(defun run-tests ()
  (let ((results (fiveam:run 'c-parse)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(defun regex-character-class-to-esrap-liquid (regex)
  (lex-rule-dump (parse-with-garbage 'lex-rule-character-class regex)))
(fiveam:test what
  (fiveam:is (char= (c-parse-parse 'lex-char-or-escaped-char "\\\\")
		    #\\))
  (fiveam:is (char= (c-parse-parse 'lex-char-or-escaped-char "\\t")
		    #\Tab))
  (fiveam:is (string= (c-parse-parse 'lex-string "\"234\\t234\"")
		      "234	234"))
  (fiveam:is (string= (char-to-escaped-char #\Newline)
		      "\\n"))
  (fiveam:is (string= (char-to-escaped-char #\s)
		      "s"))
  (fiveam:is (char= (parse-with-garbage
		     (regex-character-class-to-esrap-liquid "[^a-zA-Z_0-9]")
		     "$"))
	     #\$)
  (fiveam:is (char= (parse-with-garbage
		     (regex-character-class-to-esrap-liquid "[a-zA-Z_0-9]")
		     "S"))
	     #\S))

(defun test-parse-rules (&optional (rules *lex-rules-lines*))
  (mapcar (lambda (text)
	    (parse-with-garbage 'lex-rule-start text))
	  rules))


;;run split-lex-2 to set the dynamic variables
(defun test-lines (&optional (rule 'lex-rule-start) (rules *lex-rules-lines*))
  (let ((correct 0)
	(wrong 0))
    (terpri)
    (mapc (lambda (text)
	    (let* ((obj (parse-with-garbage rule text))
		   (a (princ-to-string 
		       obj)))
	      (flet ((dump ()
		       (princ a)
		       (terpri)
		       (princ text)
		       (terpri)))
		(cond ((string-a-prefix-b-p
			a
			text)
		       (progn
			 (format t "~%same:~%") 
			 (dump))
		       (incf correct))
		      (t
		       (incf wrong)
		       (format t "~%DIFFERENT:~%")
		       (dump)
	;	       (inspect obj)
		       )))))
	  rules)
    (format t "correct: ~a wrong: ~a~%" correct wrong)
    (list wrong correct)))

(defun teststuff ()
  (test-lines)
  (test-lines 'lex-rule-start
	      (mapcar 'spec-lex-rule-rule
		      (mapcar
		       (lambda (item)
			 (parse-with-garbage 'lex-line-def item))
		       *lex-definitions-lines*))))

(defun test-things (&optional not-pretty)
  (let ((*print-raw* not-pretty))
    (teststuff))
  (values))



;;;end
;;(string-thing lex-token-type yacc-token-type)
;;;;
(defun lex (string &optional (stream *standard-output*))
  (let ((start 0))
    (loop
       (multiple-value-bind (result len)
	   (parse-with-garbage 'lexer-foo string :start start)
	 (when (zerop len)
	   (return))
	 (destructuring-bind (string-thing ignorable yacc-token-type) result
	   (declare (ignorable string-thing yacc-token-type ignorable))
	   ;;(write-char (char-code-object yacc-token-type) stream)
	   (princ (stringy (car result)) stream)
	   )
	 (incf start len)))))

(defun lex2 (string)
  (with-output-to-string (stream)
    (lex string stream)))

;;FIXME:: hack -> using unicode characters to represent tokens, thus simplifyng tokens
#+nil
(progn
  (defparameter *char-code-pointer* nil)
  (defparameter *objects-to-characters* nil)
  (defun reset-char-code-object-table ()
    (setf *char-code-pointer* 32)
    (setf *objects-to-characters* (make-hash-table :test 'equal)))
  (reset-char-code-object-table)
  (defun char-code-object (obj)
    (let ((there? (gethash obj *objects-to-characters*)))
      (unless there?
	(let ((new (code-char *char-code-pointer*)))
	  (setf (gethash obj *objects-to-characters*)
		new)
	  (setf there? new))
	(incf *char-code-pointer*))
      there?)))

(define-c-parse-rule left-recursion? ()
  (progn-v left-recursion?
	   #\(
	   character
	   #\)))

;;32 -> 126 inclusive
;;0 -> 126 - 32 = 94
;;0 -> 94 inclusive = mod 95
(defun ascii-increment (char)
  (let ((code (char-code char)))
    (code-char (+ 32 (mod (+ 1 (- code 32)) 95)))))

#+nil
(get-directives 'per-iter
		(alexandria:read-file-into-string
		 (ensure-cached-no-directives
		  "/home/imac/install/src/emacs-mirror/emacs-master/src/xdisp.c")
		 ))
(defparameter *cpp-test-path* "/home/imac/install/src/emacs-mirror/emacs-master/src/bytecode.c")
(defun reroot-cpp (&optional (path *cpp-test-path*))
  (reroot path :prefix "_cpp_"))

(defun cpp-it (&optional (path *cpp-test-path*))
  (uiop:run-program (print (cppbar path))
		    :output *standard-output* :error-output *standard-output*))

(defun cpp-include-directories-foo ()
  (stringy (mapcar (lambda (x)
		     (format nil " -I~a " x))
		   *include-directories*)))

(defparameter *gnu-compiler-builtins-header* 
  (merge-pathnames "test/GNU_compiler_builtins.h" *path*))
(defparameter *gnu-compiler-builtins-header-include-flag*
  (format nil " -include ~a " (uiop:unix-namestring *gnu-compiler-builtins-header*)))
(defun cppbar (&optional (path *cpp-test-path*))
  (let ((infile (uiop:unix-namestring path))
	(outfile (uiop:unix-namestring (reroot-cpp path)))
	(flags (stringy
		(list
		 ;;from man cpp
		 ;;" -CC " ;;preserve comments
		 " -P " ;;no line information
		 *gnu-compiler-builtins-header-include-flag*
		 "-std=c99"
		 ;;" -E "
		 #+nil
		 "-fdirectives-only " ;; do not expand macros
		 ;;"-fdebug-cpp" ;;token information?
		 ))))
    (format nil "cpp ~a ~a ~a -o ~a " (cpp-include-directories-foo) flags infile outfile)))

(defparameter *pycparser-src-path* "/home/imac/install/src/pycparser-master/")
(defparameter *pycparser-c-ast-cfg*
  (merge-pathnames "pycparser/_c_ast.cfg" *pycparser-src-path*))

(deflazy:deflazy pycparser-c-ast-cfg ()
  (remove-if
   (lambda (x)
     ;;;comment lines start with #
     (char= (aref x 0)
	    #\#
	    ))
   (file-lines-no-whitespace-lines
    (alexandria:read-file-into-string *pycparser-c-ast-cfg*))))

(defun compile-lex-def-to-esrap-liquid (&optional (rule-string "[a-zA-Z]*yolo"))
  (lex-rule-dump
   (parse-with-garbage 'lex-rule-start rule-string)))

(defun wot89 ()
  `(progn
     (define-c-parse-rule pycparser-cfg-name ()
       (stringy ,(compile-lex-def-to-esrap-liquid "[a-zA-Z_]+")))))
(define-c-parse-rule pycparser-cfg ()
  (let ((name (v pycparser-cfg-name)))
    (list*
     name
     (progn-v
      #\:
      #\Space
      (progm #\[
	     pycparser-c-ast-cfg-params
	     #\])))))
(define-c-parse-rule pycparser-c-ast-cfg-params ()
  (remove
   nil
   (times
    (||
     (list-v pycparser-cfg-name
	     (list-length (times #\*)))
     (progn (v ", ") nil)))))
(deflazy pycparser-cfg-ast-nodes ()
  (eval (wot89))
  (values))
(defun parse-pycparser-c-ast-def (&optional (string "Union: [name, decls**]"))
  (getfnc 'pycparser-cfg-ast-nodes)
  (parse-with-garbage 'pycparser-cfg string))

(defun get-parsed-c-ast-defs ()
  (let ((text (deflazy:getfnc 'pycparser-c-ast-cfg)))
    (mapcar 'parse-pycparser-c-ast-def
	    text)))

(defun dump-defstruct-from-pycparser-c-ast-def (def)
  (destructuring-bind (name &rest params) def
    (let ((name (json-to-lisp-symbol name))
	  (conc-name (utility:symbolicate2 (list (string name) ".")
					   (symbol-package name))))
      `(struct-to-clos:struct->class
	(defstruct (,name (:conc-name ,conc-name))
	  ,@
	  (mapcar 'json-to-lisp-symbol
		  (mapcar 'first params)))))))

(defun gen-pycparser-node-objets ()
  (let ((data (get-parsed-c-ast-defs)))
    `(progn
       ,@(mapcar 'dump-defstruct-from-pycparser-c-ast-def data))))

;;(defun eval-pycparser-node-objects )

;;ripped from pycparser-master/pycparser/_c_ast.cfg
;;#   <name>*     - a child node
;;#   <name>**    - a sequence of child nodes
;;#   <name>      - an attribute

;;;;lispify and delispify are inverses
(defun lispify (string)
  ;;;prefix all uppercase letters with a dash, and make lowercase
  (let ((acc nil))
    (dotimes (index (length string))
      (let ((char (aref string index)))
	(if (upper-case-p char)
	    (progn
	      (push #\- acc)
	      (push (char-downcase char) acc))
	    (push char acc))))
    (string-upcase (stringy (nreverse acc)))))
(defun delispify (string)
  (let ((acc nil)
	(index 0)
	(len (length string)))
    (while (> len index)
      (let ((char (aref string index)))
	(if (char= #\- char)
	    (progn
	      (push (char-upcase (aref string (+ 1 index))) acc)
	      (incf index 2))
	    (progn (push (char-downcase char) acc)
		   (incf index 1)))))
    (stringy (nreverse acc))))

(progn
  (defparameter *json-to-lisp-names*
    (make-hash-table :test 'equal))
  (defparameter *lisp-names-to-json*
    (make-hash-table :test 'eq)))
(defparameter *json-name-package* (or (find-package :json-name)
				      (make-package :json-name :use '(:cl))))
(defun make-and-export-sym (string package)
  (let ((sym (intern string
		     package)))
    (export
     sym
     package)
    sym))
(defun json-to-lisp-symbol (string &optional (hash *json-to-lisp-names*)
				     (package *json-name-package*)
				     (otherhash *lisp-names-to-json*))
  (symbol-macrolet ((place (gethash string hash)))
    (or place
	(let* ((newstr (lispify string))
	       (sym (make-and-export-sym newstr package)))
	  (setf place sym)
	  (setf (gethash sym otherhash) string)
	  sym))))
(defun lisp-symbol-to-json (sym &optional (otherhash *lisp-names-to-json*))
  (or (gethash sym otherhash)
      (error "no associated json name: ~a" sym)))
