(cl:in-package #:path-string)

(setf (documentation '*sep* 'variable)
      "The platform-specific file separator, '\\' or '/'.")

(setf (documentation '*delimiter* 'variable)
      "The platform-specific path delimiter, ';' or ';'.")

(setf (documentation 'normalize 'function)
      "Normalize a path string, taking care of '..' and '.' parts.")

(setf (documentation 'join 'function)
      "Join all arguments together and normalize the resulting path.")

(setf (documentation 'resolve 'function)
      "Resolves the last argument to an absolute path")

(setf (documentation 'absolute-p 'function)
      "Determines whether the path is an absolute path. An absolute path will
       always resolve to the same location, regardless of the working directory.")

(setf (documentation 'relative 'function)
      "Solve the relative path from FROM to TO.")

(setf (documentation 'dirname 'function)
      "Return the directory name of a path. Similar to the Unix dirname command.")

(setf (documentation 'basename 'function)
      "Return the last portion of a path. Similar to the Unix baseline command.")

(setf (documentation 'extname 'function)
      "Return the extension of the path, from the last '.' to the end of the
       string in the last portion of the path. If there is no '.' in the last
       portion of the path or the first character of it is '.', then then return
        an empty string.")

(setf (documentation 'parse 'function)
      "Returns a list of path pieces (ROOT DIR BASE EXT NAME) from a path string.")
