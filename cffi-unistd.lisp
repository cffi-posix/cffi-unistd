;;
;;  cffi-unistd  -  Common Lisp wrapper for unistd.h
;;
;;  Copyright 2017 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :cffi-unistd)

(defcfun ("access" c-access) :int
  (name :string)
  (type :int))

(defun access (name type)
  (let ((r (c-access name type)))
    (when (< r 0)
      (error-errno "access"))
    r))

(defcfun ("lseek" c-lseek) off-t
  (fd :int)
  (offset off-t)
  (whence :int))

(defun lseek (fd offset whence)
  (let ((r (c-lseek fd offset whence)))
    (when (< r 0)
      (error-errno "lseek"))
    r))

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
    (let ((r (c-pipe fd)))
      (when (< r 0)
	(error-errno "pipe"))
      (values (mem-aref fd :int 0)
	      (mem-aref fd :int 1)))))

(defmacro with-pipe ((in-var out-var) &body body)
  `(multiple-value-bind (,in-var ,out-var) (pipe)
     (unwind-protect (progn ,@body)
       (close ,out-var)
       (close ,in-var))))
