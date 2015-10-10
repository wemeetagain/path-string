(cl:in-package #:asdf-user)

(defsystem :path-string-test
  :description "path-string unit tests"
  :license "MIT"
  :depends-on (:path-string :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "t"
		:components
		((:test-file "posix"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
