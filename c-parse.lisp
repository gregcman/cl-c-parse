(in-package :c-parse)

(defparameter *path* (asdf:system-source-directory :c-parse))
;;generated via grepping .h and .c files for "#include <"
(defun whitespace-string (str)
  "return t if its all spaces or empty"
  (dotimes (i (length str))
    (unless (char= #\Space (aref str i))
      (return-from whitespace-string nil)))
  t)
(defun file-lines-no-whitespace-lines (string)
  (remove-if #'whitespace-string
	     (split-sequence:split-sequence #\Newline string)))
(defun print-list (&optional (data *lex-txt2*))
  (dolist (item data)
    (print item)))
(defun princ-list (&optional (data *lex-txt2*))
  (dolist (item data)
    (terpri)
    (princ item)))

;;both the lex and yacc file are separated into 3 sections by two "%%"
;;for use with *lex-txt2* and *yacc-txt2*
(defun %%-positions (data)
  (let ((first-end (position "%%" data :test 'string=)))
    (values first-end
	    (position "%%" data :test 'string= :start (+ 1 first-end)))))

(define-esrap-env c-parse)
(define-c-parse-rule lex-yacc-token-char ()
  (|| #\_
      (character-ranges
       (#\a #\z)
       (#\A #\Z))))
(define-c-parse-rule lex-yacc-token ()
  (postimes lex-yacc-token-char))

(defun stringify (seq)
  "coerce sequence into a string"
  (coerce seq 'string))
(define-c-parse-rule lex-token-string ()
  ;;happens to be same for yacc. FIXME:: proper names for things?
  (stringify (v lex-yacc-token)))

(defmacro parse-with-garbage (rule text &rest rest &key &allow-other-keys)
  `(c-parse-parse ,rule ,text :junk-allowed t ,@rest))

(defun stringy (tree)
  ;;turn a tree of nil's and characters produced by esrap-liquid into a
  ;;string
  (with-output-to-string (stream)
    (labels ((rec (node)
	       (when node
		 (if (atom node)
		     (princ node stream)
		     (progn (rec (car node))
			    (rec (cdr node)))))))
      (rec tree))))

(defun concatenate-string (&rest rest)
  (%concatenate-string rest))
(defun %concatenate-string (rest)
  (apply 'concatenate 'string rest))

;;;yacc and lex comments are the same?
(define-c-parse-rule lex-yacc-multiline-comment ()
  (progn-v
   "/*"
   lex-comment-end))
(define-c-parse-rule lex-comment-end-token ()
  (progn (v #\*)
	 (v #\/)))
(define-c-parse-rule lex-comment-end ()
  (prog1 (postimes
	  (progn (! lex-comment-end-token)
		 (v character))
	  )
    (v lex-comment-end-token))
   nil
   )

;;;;for default testing purposes
(defparameter *emacs-src-root-path* "/home/imac/install/src/emacs-mirror/emacs-master/")
(defun emacsify-path (&optional (path "src/lisp.h"))
  (merge-pathnames path *emacs-src-root-path*))

(defparameter *testpath*
  #+nil
  (emacsify-path
   (merge-pathnames
    ;"lisp.h"
    "syntax.h"
    ;"keymap.h"
    "src/"))
  #+nil
  "/home/imac/install/src/pycparser-master/examples/c_files/funky.c"
  ;;#+nil
  "/home/imac/install/src/pycparser-master/examples/c_files/hash.c")

;;FIXME:: where to put test files?
(defparameter *text-test-file*
  (alexandria:read-file-into-string
   *testpath*))

(defmacro while (condition &body body)
  `(do () ((not,condition))
     ,@body))

(defun symbol= (sym1 sym2)
  (eq sym1 sym2))

(defgeneric equalp? (a b))
(defmethod equalp? ((a number) (b number))
  (= a b))
(defmethod equalp? ((a cons) (b cons))
  (and (equalp? (car a) (car b))
       (equalp? (cdr a) (cdr b))))
(defmethod equalp? ((a t) (b t))
  nil)
(defmethod equalp? ((a character) (b character))
  (char= a b))
(defmethod equalp? ((a string) (b string))
  (string= a b))
(defmethod equalp? ((a symbol) (b symbol))
  (symbol= a b))

(defparameter *include-directories*
  (mapcar 'emacsify-path
	  '("src/"
	    "lib/")))

(defparameter *c-file-type-strings*
  '("c" "h"))
;;;FIXME::put in the filesystem file
(defun c-filetype-p (path)
  (let ((type (pathname-type path)))
    (find type *c-file-type-strings* :test 'string=)))

(defun map-c-files-in-directory (&key (fun 'print)
				   (path (first *include-directories*)))
  (let* ((files (uiop:directory-files path))
	 (only-c-files (remove-if-not 'c-filetype-p files)))
    (mapc fun only-c-files)))

(defun get-c-files (&optional (path *emacs-src-root-path*))
  (let ((acc nil))
    (uiop:collect-sub*directories
     path
     (constantly t)
     (constantly t)
     (lambda (subdir)
       (map-c-files-in-directory :path subdir	
				 :fun
				 (lambda (file)
				   (push file acc)))))
    (return-from get-c-files (nreverse acc))))

(defun total-emacs-c-bytes (&optional (path *emacs-src-root-path*))
  (total-file-bytes (get-c-files path)))

(defun total-file-bytes (files-list)
  (reduce '+
	  (mapcar (lambda (path)
		    (osicat-posix:stat-size
		     (osicat-posix:stat path)))
		  files-list)))

(defun find-just-before (item list fun &rest rest &key &allow-other-keys)
  (let ((position (apply 'position-if
			 (lambda (x)
			   (funcall fun x item))
			 list rest)))
    (nthcdr position list)))

(defun round-off (x size)
  (* size (round (/ x size))))

(defun get-bytesize (bytes)
  (let* ((exponent (1- (log bytes 1024)))
	 (data (first (find-just-before
		       exponent
		       '(
			 (0 "Bytes")
			 (1 "Kilobytes")
			 (2 "Megabytes")
			 (3 "Gigabytes")
			 (4 "Terabytes")
			 ;;FIXME::add more sizes
			 )
		       '>
		       :key 'first))))
    (destructuring-bind (exponent name) data
      (list (round-off (utility:floatify (/ bytes (expt 1024 exponent)))
		       0.25)
	    name))))

(defun how-big-is-emacs-c-code? (&optional (path *emacs-src-root-path*))
  (destructuring-bind (num name) (get-bytesize (total-emacs-c-bytes path))
    (format t "~%~a ~a~%" num name))
  (values))

(defun emacs-c-source ()
  (apply 'nconc
	 (mapcar 'get-c-files *include-directories*)))

;#+nil ;;non-parallel way
(defun cached-emacs-c-files ()
  (mapcar 'ensure-cached-token-intervals
	  (emacs-c-source)))
;;FIXME:: lazy load lparallel kernel with deflazy?
#+nil
(setf lparallel:*kernel* (lparallel:make-kernel 4))
#+nil
(defun submit-emacs-jobs ()
  (lparallel:pmapc 'ensure-cached-token-intervals
		   (emacs-c-source)))
