(cl:in-package #:asdf-user)

(defsystem :path-string
  :version (:read-file-form "version.txt")
  :description "A path utility library"
  :author "Cayman Nava"
  :license "MIT"
  :depends-on (:cl-ppcre :split-sequence :uiop)
  :components ((:module "src"
		:serial t
		:components
		((:file "util")
		 (:file "posix")
		 (:file "path-string")
		 (:file "documentation"))))
  :long-description #.(uiop:read-file-string
		       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op path-string-test))))
