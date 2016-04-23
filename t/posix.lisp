(cl:in-package #:cl-user)

(defpackage #:path-string-test
  (:use #:cl #:prove #:path-string))
(in-package #:path-string-test)

(plan nil)

(subtest "normalize"
  (let ((inputs '("/foo/bar//baz/asdf/quux/.."
                  "/foo/bar/./baz/asdf/../quux"))
        (outputs '("/foo/bar/baz/asdf"
                   "/foo/bar/baz/quux")))
    (loop for input in inputs
          for output in outputs
          do (is (normalize input) output))))

(subtest "join"
  (let ((inputs '(("/foo" "bar" "baz/asdf" "quux" "..")))
        (outputs '("/foo/bar/baz/asdf")))
    (loop for input in inputs
          for output in outputs
          do (is (apply #'join input) output))))

(subtest "resolve"
  (let ((inputs '(("/foo/bar" "./baz")
                  ("/foo/bar" "/tmp/file/")))
        (outputs '("/foo/bar/baz"
                   "/tmp/file")))
    (loop for input in inputs
          for output in outputs
          do (is (apply #'resolve input) output))))

(subtest "absolute-p"
  (let ((inputs '("/foo/bar"
                  "/baz/.."
                  "quux/"
                  "."))
        (outputs '(t
                   t
                   nil
                   nil)))
    (loop for input in inputs
          for output in outputs
          do (is (not (not (absolute-p input))) output))))

(subtest "relative"
  (let ((inputs '(("/data/orandea/test/aaa" "/data/orandea/impl/bbb")))
        (outputs '("../../impl/bbb")))
    (loop for input in inputs
          for output in outputs
          do (is (apply #'relative input) output))))

(subtest "dirname"
  (let ((inputs '("/foo/bar/baz/asdf/quux"))
        (outputs '("/foo/bar/baz/asdf")))
    (loop for input in inputs
          for output in outputs
          do (is (dirname input) output))))

(subtest "basename"
  (let ((inputs '(("/foo/bar/baz/asdf/quux.html" nil)
                  ("/foo/bar/baz/asdf/quux.html" "")
                  ("/foo/bar/baz/asdf/quux.html" ".html")
                  ("/foo/bar/baz/asdf/quux.html" "quux.html")
                  ("/foo/bar/baz/asdf/quux.html" "morequux.html")
                  ("/home/dir/" nil)
                  ("/home/dir/" "")))
        (outputs '("quux.html"
                   "quux.html"
                   "quux"
                   ""
                   "quux.html"
                   "dir"
                   "dir")))
    (loop for (input1 input2) in inputs
          for output in outputs
          do (is (basename input1 input2) output))))

(subtest "extname"
  (let ((inputs '("index.html"
                  "index.coffee.md"
                  "index."
                  "index"
                  ".index"))
        (outputs '(".html"
                   ".md"
                   "."
                   ""
                   "")))
    (loop for input in inputs
          for output in outputs
          do (is (extname input) output))))

(subtest "parse"
  (let ((inputs '("/home/user/dir/file.txt"))
        (outputs '(("/" "/home/user/dir" "file.txt" ".txt" "file"))))
    (loop for input in inputs
          for output in outputs
          do (is-values (parse input) output))))

(finalize)
