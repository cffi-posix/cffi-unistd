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
   #:sleep
   #:write)
  (:export
   #:+f-ok+
   #:+fd-setsize+
   #:+nfdbits+
   #:+r-ok+
   #:+seek-set+
   #:+seek-cur+
   #:+seek-end+
   #:+seek-data+
   #:+seek-hole+
   #:+stderr-fileno+
   #:+stdin-fileno+
   #:+stdout-fileno+
   #:+w-ok+
   #:+x-ok+
   #:access
   #:alarm
   #:c-access
   #:c-alarm
   #:c-chdir
   #:c-chown
   #:c-close
   #:c-dup
   #:c-dup2
   #:c-dup3
   #:c-environ
   #:c-euidaccess
   #:c-fchdir
   #:c-fchown
   #:c-fchownat
   #:c-getcwd
   #:c-lchown
   #:c-lseek
   #:c-pause
   #:c-pipe
   #:c-pipe2
   #:c-pread
   #:c-pwrite
   #:c-read
   #:c-select
   #:c-sleep
   #:c-ualarm
   #:c-usleep
   #:c-write
   #:chdir
   #:chown
   #:close
   #:dup
   #:dup2
   #:dup3
   #:environ
   #:euidaccess
   #:fchdir
   #:fchown
   #:fchownat
   #:fd-clr
   #:fd-elt
   #:fd-isset
   #:fd-mask
   #:fd-set
   #:fd-set-filter
   #:fd-zero
   #:fds-bits
   #:getcwd
   #:gid-t
   #:intptr-t
   #:lchown
   #:list-to-fd-set
   #:lseek
   #:off-t
   #:pause
   #:pid-t
   #:pipe
   #:pipe2
   #:pread
   #:pwrite
   #:read
   #:read-non-blocking
   #:seconds-to-timeval
   #:select
   #:size-t
   #:sleep
   #:socklen-t
   #:ssize-t
   #:timeval
   #:tv-sec
   #:tv-usec
   #:ualarm
   #:uid-t
   #:useconds-t
   #:usleep
   #:with-pipe
   #:with-selected
   #:write
   #:write-non-blocking))
