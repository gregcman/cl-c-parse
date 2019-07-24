(in-package :c-parse)

;;;;Make a subdirectory that mimics the real directories

(defparameter *cache* (merge-pathnames "shadowroot/" *path*))
(defun re-root-real-path (path &optional (base *cache*))
  "change the root of path to base, making sure that therer can be no outside reference.
only works if path actually exists."
  (let ((truename (uiop:truename* path)))
    (unless truename
	(error "does not exist ~s" path))
    (unless (uiop:absolute-pathname-p path)
      (error "not absolute:~s" path))

    ;;FIXME::hack?
    (let ((base-directory (pathname-directory base))
	  (path-directory  (pathname-directory truename)))
      (make-pathname :directory
		     (append base-directory (rest path-directory))
		     :name (pathname-name truename)
		     :type (pathname-type truename)))))

(defun reroot (path &key (suffix "")
		      (prefix "")
		      (create nil))
  (let ((reroot (re-root-real-path path)))
    (ensure-directories-exist reroot)
    ;;if its a file, touch it
    (let ((new (add-file-suffix suffix (add-file-prefix prefix reroot))))
      (when create
	(unless (uiop:directory-pathname-p new)
	  (touch-file new)))
      new)))

(defparameter *touch-test-path* (merge-pathnames "touch.txt" *path*))
(defun touch-file (&optional (path *touch-test-path*))
  (with-open-file (stream path :if-does-not-exist :create)))

;;be able to make a derived filename
#+nil
(defun pathname-name-and-type (&optional (path *touch-test-path*))
  (let ((name (pathname-name path))
	(type (pathname-type path)))
    (if (or name type)
	(concatenate-string
	 name
	 (if type
	     "."
	     nil)
	 type))))
(defun get-directory (&optional (path *testpath*))
  (make-pathname :directory (pathname-directory path)))
(defun add-file-extension (extension-fun &optional (path *testpath*))
  (let ((dir (get-directory path)))
    (merge-pathnames
     (make-pathname :name
		    (funcall extension-fun (pathname-name path))
		    :type (pathname-type path))
     dir)))

;;(ADD-FILE-SUFFIX "~") lisp.h -> ~lisp.h
(defun add-file-suffix (suffix &optional (path *testpath*))
  (add-file-extension (lambda (x)
			(concatenate-string x suffix))
		      path))
;;(ADD-FILE-SUFFIX ".directive") lisp.h -> lisp.h.directive
(defun add-file-prefix (prefix &optional (path *testpath*))
  (add-file-extension (lambda (x)
			(concatenate-string prefix x))
		      path))
