(asdf:defsystem #:c-parse
  :author "terminal625"
  :license "MIT"
  :description "processed c parser"
  :depends-on (#:alexandria
	       #:split-sequence
	       #:uiop
	       #:esrap-liquid
	       #:utility
	       #:yacc
	       #:uncommon-lisp
	       #:trivia
	       #:fiveam
	       #:deflazy
	       #:osicat ;;for seeing size of files
	       ;;#:lparallel ;;for using multiple cores to lex
	       )
  :serial t
  :components 
  ((:file "package")
   (:file "c-parse")
   (:file "lex")
   (:file "lex-txt")
   (:file "yacc")
   (:file "filesystem")
   (:file "preprocessor")
   ;;(:file "test")
   ))
