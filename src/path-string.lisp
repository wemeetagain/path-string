(cl:in-package #:cl-user)

(defpackage #:path-string
  (:use #:cl
	#+unix #:path-string.posix
	#+windows #:path-string.windows)
  (:nicknames :path)
  (:import-from #:cl-ppcre
		#:register-groups-bind)
  (:import-from #:split-sequence
		#:split-sequence)
  (:import-from #:uiop
	       #:absolute-pathname-p
	       #:pathname-directory-pathname)
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
