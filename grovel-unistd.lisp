
(in-package :cffi-unistd)

(include "unistd.h")

(constant (+stdin-fileno+ "STDIN_FILENO"))
(constant (+stdout-fileno+ "STDOUT_FILENO"))
(constant (+stderr-fileno+ "STDERR_FILENO"))

(ctype ssize-t "ssize_t")
(ctype size-t "size_t")
(ctype gid-t "gid_t")
(ctype uid-t "uid_t")
(ctype off-t "off_t")
(ctype off64-t "off64_t")
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
