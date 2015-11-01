(cl:in-package #:cl-user)

(defpackage #:path-string.posix
  (:use #:cl)
  (:import-from #:split-sequence
		#:split-sequence)
  (:import-from #:uiop
		#:absolute-pathname-p
		#:getcwd
		#:pathname-directory-pathname)
  (:import-from #:path-string.util
		#:posix-split-path
		#:normalize-list
		#:trim-list
		#:posix-join-components
		#:posix-format-path)
  (:export #:*sep*
	   #:*delimiter*)
  (:export #:normalize
	   #:join
	   #:resolve
	   #:absolute-p
	   #:relative
	   #:dirname
	   #:basename
	   #:extname
	   #:parse))

(in-package #:path-string.posix)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *sep* #\/)

  (defvar *delimiter* #\:))

(defun normalize (path)
  (let ((absolute-p (absolute-p path))
	(trailing-slash (and (not (string= path ""))
			     (char= (aref path (1- (length path)))
				    #\/))))
    (setf path
	  (posix-join-components
	   (normalize-list
	    (split-sequence #\/ path)
	    (not absolute-p))))

    (when (and (string= path "")
	       absolute-p)
      (setf path "."))
    (when (and (not (string= path ""))
	       trailing-slash)
      (setf path (format nil "~A/" path)))

    (posix-format-path path absolute-p)))

(defun join (&rest paths)
  (normalize
   (apply #'concatenate 'string
	  (loop
	     with path-exists = nil
	     for segment in paths
	     unless (string= segment "")
	     if path-exists
	     collect "/" end and
	     collect segment and
	     do (setf path-exists t)))))

(defun resolve (&rest paths)
  (let ((resolved-path "")
	(resolved-absolute nil))
    (flet ((update-path (path)
	     (unless (string= path "")
	       (setf resolved-path
		     (format nil "~A/~A" path resolved-path))
	       (setf resolved-absolute
		     (char= (aref path 0) #\/)))))
      (loop
	 for path in (nreverse paths)
	 for resolved-absolute = (update-path path)
	 if resolved-absolute
	 return (values)
	 finally (update-path (namestring (getcwd))))
      (or (posix-format-path
	   (posix-join-components
	    (normalize-list
	     (split-sequence #\/ resolved-path)
	     (not resolved-absolute)))
	   resolved-absolute)
	  "."))))

(defun absolute-p (path)
  (absolute-pathname-p path))

(defun relative (from to)
  (let* ((from (subseq (resolve from) 1))
	 (to (subseq (resolve to) 1))
	 (from-parts (trim-list
		      (split-sequence #\/ from)))
	 (to-parts (trim-list
		    (split-sequence #\/ to)))
	 (length (min (length from-parts) (length to-parts)))
	 (same-parts-length (loop for i below length
			       unless (string= (nth i from-parts)
					       (nth i to-parts))
			       return i
			       finally (return length))))
    (posix-join-components
     (append (loop for i below (- (length from-parts) same-parts-length)
		collect "..")
	     (subseq to-parts same-parts-length)))))

(defun dirname (path)
  (multiple-value-bind (root dir basename ext)
      (posix-split-path path)
    (declare (ignore basename ext))
    (if (and (string= root "")
	     (string= dir ""))
	"."
	(concatenate 'string
		     root
		     (unless (string= dir "")
		       (subseq dir 0 (1- (length dir))))))))

(defun basename (path)
  (multiple-value-bind (root dir base)
      (posix-split-path path)
    (declare (ignore root dir))
    base))

(defun extname (path)
  (multiple-value-bind (root dir base ext)
      (posix-split-path path)
    (declare (ignore root dir base))
    ext))

(defun parse (path)
  (multiple-value-bind (root dir base ext)
      (posix-split-path path)
    (let ((dir (posix-format-path
		(subseq dir 0 (- (length dir) 1))
		(string= root "/")))
	  (name (subseq base 0 (- (length base) (length ext)))))
      (values root dir base ext name))))
