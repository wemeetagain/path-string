(cl:in-package #:cl-user)

(defpackage #:path-string
  (:use #:cl
        #+unix #:path-string.posix
        #+windows #:path-string.windows)
  (:export #:*sep*
           #:*delimiter*
           #:normalize
           #:join
           #:resolve
           #:absolute-p
           #:relative
           #:dirname
           #:basename
           #:extname
           #:parse))
