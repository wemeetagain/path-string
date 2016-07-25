(cl:in-package #:cl-user)

(defpackage :path-string.util
  (:use #:cl)
  (:import-from #:cl-ppcre
                #:register-groups-bind)
  (:export #:normalize-list
           #:trim-list
           #:*posix-split-path-re*
           #:posix-split-path
           #:posix-join-components
           #:posix-format-path))

(in-package #:path-string.util)

(defun normalize-list (parts &optional allow-above-root-p)
  (nreverse
   (loop
     with result = '()
     for part in parts
     unless (or (string= part "")
                (string= part "."))
       if (string= part "..")
         do (cond
              ((and (length result)
                    (not (string= (nth (1- (length result)) result) "..")))
               (pop result))
              (allow-above-root-p
               (push ".." result)))
         else do (push part result)
     finally (return result))))

(defun trim-list (list)
  (let* ((last-index (1- (length list)))
         (start (loop for i upto last-index
                      unless (string= (nth i list) "")
                        return i
                      finally (return last-index)))
         (end (loop for i downfrom last-index
                    unless (string= (nth i list) "")
                      return i
                    finally (return 0))))
    (unless (> start end)
      (subseq list start (1+ end)))))

;;; posix-specific

(defparameter *posix-split-path-re*
  "^(\/?|)([\\s\\S]*?)((?:\\.{1,2}|[^\/]+?|)(\\.[^.\/]*|))(?:[\/]*)$")

(defun posix-split-path (path)
  (register-groups-bind (root dir basename ext)
      (*posix-split-path-re* path)
    (values root dir basename ext)))

(defun posix-join-components (components)
  (format nil "~{~A~^/~}" components))

(defun posix-format-path (path absolute-p)
  (format nil "~A~A"
          (if absolute-p
              "/"
              "")
          path))
