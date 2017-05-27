
(in-package :cffi-unistd)

(defcfun ("close" c-close) :int
  (fd :int))

(defun close (fd)
  (let ((r (c-close fd)))
    (when (< r 0)
      (error-errno "close"))
    r))

(defcfun ("read" c-read) ssize-t
  (fd :int)
  (buf :pointer)
  (count size-t))

(defun read (fd buf count)
  "Reads at most COUNT bytes from FD into BUF.
Returns number of bytes read."
  (let ((r (c-read fd buf count)))
    (when (< r 0)
      (error-errno "read"))
    r))

(defun read-non-blocking (fd buf count)
  "Reads at most COUNT bytes from FD into BUF.
Returns NIL if read would block,
or number of bytes read otherwise."
  (let ((r (c-read fd buf count)))
    (if (< r 0)
	(if (or (= +eagain+ errno) (= +ewouldblock+ errno))
	    nil
	    (error-errno "read"))
	r)))

(defcfun ("write" c-write) ssize-t
  (fd :int)
  (buf :pointer)
  (count size-t))

(defun write (fd buf count)
  "Writes at most COUNT bytes from BUF into FD.
Returns number of bytes written."
  (let ((r (c-write fd buf count)))
    (when (< r 0)
      (error-errno "write"))
    r))

(defun write-non-blocking (fd buf count)
  "Writes at most COUNT bytes from BUF into FD.
Returns NIL if write would block,
or number of bytes written otherwise."
  (let ((r (c-write fd buf count)))
    (if (< r 0)
	(if (or (= +eagain+ errno) (= +ewouldblock+ errno))
	    nil
	    (error-errno "write"))
	r)))

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
