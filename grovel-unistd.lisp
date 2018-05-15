;;
;;  cffi-unistd  -  Common Lisp wrapper for unistd.h
;;
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com>
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

(include "sys/select.h")
(include "sys/socket.h")
(include "unistd.h")

(constant (+stdin-fileno+ "STDIN_FILENO"))
(constant (+stdout-fileno+ "STDOUT_FILENO"))
(constant (+stderr-fileno+ "STDERR_FILENO"))

(ctype ssize-t "ssize_t")
(ctype size-t "size_t")
(ctype gid-t "gid_t")
(ctype uid-t "uid_t")
(ctype off-t "off_t")
(ctype useconds-t "useconds_t")
(ctype pid-t "pid_t")
(ctype intptr-t "intptr_t")
(ctype socklen-t "socklen_t")

(constant (+r-ok+ "R_OK"))
(constant (+w-ok+ "W_OK"))
(constant (+x-ok+ "X_OK"))
(constant (+f-ok+ "F_OK"))

(constant (+seek-set+ "SEEK_SET"))
(constant (+seek-cur+ "SEEK_CUR"))
(constant (+seek-end+ "SEEK_END"))
(constant (+seek-data+ "SEEK_DATA"))
(constant (+seek-hole+ "SEEK_HOLE"))

(ctype fd-mask "fd_mask")
(constant (+fd-setsize+ "FD_SETSIZE"))
(constant (+nfdbits+ "NFDBITS"))
