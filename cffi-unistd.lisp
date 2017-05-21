
(in-package :cffi-unistd)

(defcfun ("close" c-close) :int
  (fd :int))

(defun close (fd)
  (when (< (c-close fd) 0)
    (error-errno "close")))

(defcfun ("read" c-read) ssize-t
  (fd :int)
  (buf :pointer)
  (count size-t))

(defcfun ("write" c-write) ssize-t
  (fd :int)
  (buf :pointer)
  (count size-t))

(defcfun ("pipe" c-pipe) :int
  (pipefd (:pointer :int)))

(defun pipe ()
  (with-foreign-object (fd :int 2)
    (c-pipe fd)
    (values (mem-aref fd :int 0)
	    (mem-aref fd :int 1))))

(defmacro with-pipe ((in-var out-var) &body body)
  `(multiple-value-bind (,in-var ,out-var) (pipe)
     (unwind-protect (progn ,@body)
       (close ,out-var)
       (close ,in-var))))
