
(in-package :common-lisp)

(defpackage :cffi-unistd
  (:nicknames :unistd)
  (:use
   :cffi
   :common-lisp
   :errno)
  (:shadow
   #:close)
  (:export
   #:c-read
   #:c-close
   #:close
   #:c-write
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
