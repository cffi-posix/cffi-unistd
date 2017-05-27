
(in-package :common-lisp)

(defpackage :cffi-unistd
  (:nicknames :unistd)
  (:use
   :cffi
   :common-lisp
   :errno)
  (:shadow
   #:close
   #:read
   #:write)
  (:export
   #:c-read
   #:read
   #:read-non-blocking
   #:c-close
   #:close
   #:c-write
   #:write
   #:write-non-blocking
   #:c-pipe
   #:pipe
   #:with-pipe
   #:fd-stream
   #:fd-stream-error
   #:fd-stream-closed-error
   #:+buffer-size+
   #:fd-input-stream
   #:stream-input
   #:fd-output-stream
   #:stream-output
   #:pipe-stream
   #:fd-io-stream
   #:dual-pipe-stream))
