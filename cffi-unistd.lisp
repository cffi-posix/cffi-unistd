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

(deftype file-descriptor ()
  '(unsigned-byte 31))

(defcfun ("access" c-access) :int
  (name :string)
  (type :int))

(defun access (name type)
  (let ((r (c-access name type)))
    (when (< r 0)
      (error-errno "access"))
    r))

(defcfun ("euidaccess" c-euidaccess) :int
  (name :string)
  (type :int))

(defun euidaccess (name type)
  (let ((r (c-euidaccess name type)))
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
Returns :NON-BLOCKING if read would block,
or number of bytes read otherwise."
  (let ((r (c-read fd buf count)))
    (if (< r 0)
        (if (or (= +eagain+ errno) (= +ewouldblock+ errno))
            :non-blocking
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
Returns :NON-BLOCKING if write would block,
or number of bytes written otherwise."
  (let ((r (c-write fd buf count)))
    (if (< r 0)
        (if (or (= +eagain+ errno) (= +ewouldblock+ errno))
            :non-blocking
            (error-errno "write"))
        r)))

(defcfun ("pread" c-pread) ssize-t
  (fd :int)
  (buf :pointer)
  (nbytes size-t)
  (offset off-t))

(defun pread (fd buf nbytes offset)
  "Reads at most COUNT bytes from FD into BUF at position OFFSET
without changing file pointer. Returns number of bytes read."
  (let ((r (c-pread fd buf nbytes offset)))
    (when (< r 0)
      (error-errno "pread"))
    r))

(defcfun ("pwrite" c-pwrite) ssize-t
  (fd :int)
  (buf :pointer)
  (n size-t)
  (offset off-t))

(defun pwrite (fd buf count offset)
  "Writes at most COUNT bytes from BUF into FD at position OFFSET
without changing the file pointer. Returns number of bytes written."
  (let ((r (c-pwrite fd buf count offset)))
    (when (< r 0)
      (error-errno "pwrite"))
    r))

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
  (let ((in (gensym "IN-"))
        (out (gensym "OUT-")))
    `(multiple-value-bind (,in ,out) (pipe)
       (unwind-protect (let ((,in-var ,in)
                             (,out-var ,out))
                         ,@body)
         (close ,out)
         (close ,in)))))

(defcfun ("pipe2" c-pipe2) :int
  (pipefd (:pointer :int))
  (flags :int))

(defun pipe2 (flags)
  (with-foreign-object (fd :int 2)
    (let ((r (c-pipe2 fd flags)))
      (when (< r 0)
        (error-errno "pipe2"))
      (values (mem-aref fd :int 0)
              (mem-aref fd :int 1)))))

(defmacro with-pipe2 ((in-var out-var flags) &body body)
  (let ((in (gensym "IN-"))
        (out (gensym "OUT-")))
    `(multiple-value-bind (,in ,out) (pipe2 ,flags)
       (unwind-protect (let ((,in-var ,in)
                             (,out-var ,out))
                         ,@body)
         (close ,out)
         (close ,in)))))

(defcfun ("alarm" c-alarm) :unsigned-int
  (seconds :unsigned-int))

(defun alarm (seconds)
  (c-alarm seconds))

(defcfun ("sleep" c-sleep) :unsigned-int
  (seconds :unsigned-int))

(defun sleep (seconds)
  (c-sleep seconds))

(defcfun ("ualarm" c-ualarm) useconds-t
  (value useconds-t)
  (interval useconds-t))

(defun ualarm (value interval)
  (c-ualarm value interval))

(defcfun ("usleep" c-usleep) useconds-t
  (useconds useconds-t))

(defun usleep (useconds)
  (c-usleep useconds))

(defcfun ("pause" c-pause) :int)

(defun pause ()
  (c-pause))

(defcfun ("chown" c-chown) :int
  (file :string)
  (owner uid-t)
  (group gid-t))

(defun chown (path owner group)
  (let ((r (c-chown path owner group)))
    (when (< r 0)
      (error-errno "chown"))
    r))

(defcfun ("fchown" c-fchown) :int
  (fd :int)
  (owner uid-t)
  (group gid-t))

(defun fchown (fd owner group)
  (let ((r (c-fchown fd owner group)))
    (when (< r 0)
      (error-errno "fchown"))
    r))

(defcfun ("lchown" c-lchown) :int
  (file :string)
  (owner uid-t)
  (group gid-t))

(defun lchown (path owner group)
  (let ((r (c-lchown path owner group)))
    (when (< r 0)
      (error-errno "lchown"))
    r))

(defcfun ("fchownat" c-fchownat) :int
  (fd :int)
  (file :string)
  (owner uid-t)
  (group gid-t)
  (flag :int))

(defun fchownat (fd path owner group flag)
  (let ((r (c-fchownat fd path owner group flag)))
    (when (< r 0)
      (error-errno "fchownat"))
    r))

(defcfun ("chdir" c-chdir) :int
  (path :string))

(defun chdir (path)
  (let ((r (c-chdir path)))
    (when (< r 0)
      (error-errno "chdir"))
    r))

(defcfun ("fchdir" c-fchdir) :int
  (fd :int))

(defun fchdir (fd)
  (let ((r (c-fchdir fd)))
    (when (< r 0)
      (error-errno "fchdir"))
    r))

(defcfun ("getcwd" c-getcwd) :string
  (buf :string)
  (size size-t))

(defvar *path-max* 4096)

(defun getcwd ()
  (let* ((len (or (ignore-errors (the integer *path-max*))
                  4096))
         (s (make-string len)))
    (let ((r (c-getcwd s len)))
      (unless r
        (error-errno "getcwd"))
      r)))

(defcfun ("dup" c-dup) :int
  (oldfd :int))

(defun dup (oldfd)
  (let ((r (c-dup oldfd)))
    (when (< r 0)
      (error-errno "dup"))
    r))

(defcfun ("dup2" c-dup2) :int
  (fd :int)
  (fd2 :int))

(defun dup2 (fd fd2)
  (let ((r (c-dup2 fd fd2)))
    (when (< r 0)
      (error-errno "dup2"))
    r))

(defcfun ("dup3" c-dup3) :int
  (fd :int)
  (fd2 :int)
  (flags :int))

(defun dup3 (fd fd2 flags)
  (let ((r (c-dup3 fd fd2 flags)))
    (when (< r 0)
      (error-errno "dup3"))
    r))

(defcvar ("environ" c-environ) (:pointer :string)
  "NULL-terminated array of \"NAME=VALUE\" environment variables.")

(defmacro do-environ ((var) &body body)
  (let ((env (gensym "ENV-")))
    `(let ((,env c-environ))
       (loop
          (let ((,var (mem-ref ,env :string)))
            (unless ,var
              (return))
            ,@body)
          (setf ,env (mem-aptr ,env '(:pointer :string) 1))))))

(defun environ ()
  (let ((list ()))
    (do-environ (env)
       (push env list))
    list))

#+test
(environ)

(defun getenv (name)
  (let* ((name= (concatenate 'string name "="))
         (len (length name=)))
    (do-environ (env)
      (when (string= name= env :end2 len)
        (return (subseq env len))))))

#+test
(getenv "HOME")

(defcfun ("execve" c-execve) :int
  (path :string)
  (argv (:pointer :string))
  (envp (:pointer :string)))

(defun execve (path argv envp)
  (let ((r (c-execve path argv envp)))
    (if (= -1 r)
        (error-errno path)
        (error "execve ~S" path))))

(defcfun ("execv" c-execv) :int
  (path :string)
  (argv (:pointer :string)))

(defun execv (path argv)
  (let ((r (c-execv path argv)))
    (if (= -1 r)
        (error-errno path)
        (error "execv ~S" path))))

(defcfun ("execvp" c-execvp) :int
  (path :string)
  (argv (:pointer :string)))

(defun execvp (path argv)
  (let ((r (c-execvp path argv)))
    (if (= -1 r)
        (error-errno path)
        (error "execv ~S" path))))

(defcfun ("execvpe" c-execvpe) :int
  (path :string)
  (argv (:pointer :string))
  (envp (:pointer :string)))

(defun execvpe (path argv envp)
  (let ((r (c-execvpe path argv envp)))
    (if (= -1 r)
        (error-errno path)
        (error "execvpe ~S" path))))

(defcfun ("nice" c-nice) :int
  (inc :int))

(defun nice (inc)
  (setf errno 0)
  (let ((r (c-nice inc)))
    (unless (= 0 errno)
      (error-errno "nice"))
    r))

(defcfun ("_exit" c-_exit) :void
  (status :int))

(defun _exit (status)
  (c-_exit status))

(defcfun ("pathconf" c-pathconf) :long
  (path :string)
  (name :int))

(defun pathconf (path name)
  (c-pathconf path name))

(defcfun ("sysconf" c-sysconf) :long
  (name :int))

(defun sysconf (name)
  (c-sysconf name))

(defcfun ("getpid" c-getpid) pid-t)

(defun getpid ()
  (c-getpid))

(defcfun ("getppid" c-getppid) pid-t)

(defun getppid ()
  (c-getppid))

(defcfun ("getpgrp" c-getpgrp) pid-t)

(defun getpgrp ()
  (c-getpgrp))

(defcfun ("getpgid" c-getpgid) pid-t)

(defun getpgid ()
  (c-getpgid))

(defcfun ("setpgid" c-setpgid) pid-t)

(defun setpgid ()
  (c-setpgid))

(defcfun ("setpgrp" c-setpgrp) :int)

(defun setpgrp ()
  (c-setpgrp))

(defcfun ("setsid" c-setsid) pid-t)

(defun setsid ()
  (c-setsid))

(defcfun ("getsid" c-getsid) pid-t
  (pid pid-t))

(defun getsid (pid)
  (c-getsid pid))

(defcfun ("getuid" c-getuid) pid-t)

(defun getuid ()
  (c-getuid))

(defcfun ("geteuid" c-geteuid) pid-t)

(defun geteuid ()
  (c-geteuid))

(defcfun ("getgid" c-getgid) pid-t)

(defun getgid ()
  (c-getgid))

(defcfun ("getegid" c-getegid) pid-t)

(defun getegid ()
  (c-getegid))

(defcfun ("getgroups" c-getgroups) :int
  (size :int)
  (list gid-t))

(defcfun ("setuid" c-setuid) :int
  (uid uid-t))

(defun setuid (uid)
  (c-setuid uid))

(defcfun ("seteuid" c-seteuid) :int
  (uid uid-t))

(defun seteuid (uid)
  (c-seteuid uid))

(defcfun ("setgid" c-setgid) :int
  (gid gid-t))

(defun setgid (gid)
  (c-setgid gid))

(defcfun ("setegid" c-setegid) :int
  (gid gid-t))

(defun setegid (gid)
  (c-setegid gid))

(defcfun ("fork" c-fork) pid-t)

(defun fork ()
  (let ((r (c-fork)))
    (when (= -1 r)
      (error-errno "fork"))
    r))

(defcfun ("vfork" c-vfork) pid-t)

(defun vfork ()
  (let ((r (c-vfork)))
    (when (= -1 r)
      (error-errno "vfork"))
    r))

(defcfun ("ttyname" c-ttyname) :string
  (fd :int))

(defun ttyname (fd)
  (or (c-ttyname fd)
      (error-errno "ttyname")))

(defcfun ("isatty" c-isatty) :int
  (fd :int))

(defun isatty (fd)
  (let ((r (c-isatty fd)))
    (when (= -1 r)
      (error-errno "isatty"))
    r))

(defcfun ("link" c-link) :int
  (from :string)
  (to :string))

(defun link (from to)
  (let ((r (c-link from to)))
    (when (= -1 r)
      (error-errno "link"))
    r))

(defcfun ("symlink" c-symlink) :int
  (from :string)
  (to :string))

(defun symlink (from to)
  (let ((r (c-symlink from to)))
    (when (= -1 r)
      (error-errno "symlink"))
    r))

(defcfun ("readlink" c-readlink) ssize-t
  (path :string)
  (buf :string)
  (len size-t))

(defun readlink (path)
  (let ((len *path-max*))
    (with-foreign-object (buf :char len)
      (let ((r (c-readlink path buf len)))
        (when (= -1 r)
          (error-errno path))
        (let ((octets (make-array `(,r) :element-type '(unsigned-byte 8)
                                  :initial-element 0)))
          (dotimes (i r)
            (setf (aref octets i) (mem-aref buf :unsigned-char i)))
          (babel:octets-to-string octets :encoding :utf-8))))))

#+nil
(readlink "/home/dx/common-lisp/RailsOnLisp/rol/assets")

(defcfun ("unlink" c-unlink) :int
  (name :string))

(defun unlink (name)
  (let ((r (c-unlink name)))
    (when (= -1 r)
      (error-errno "unlink"))
    r))

(defcfun ("rmdir" c-rmdir) :int
  (path :string))

(defun rmdir (path)
  (let ((r (c-rmdir path)))
    (when (= -1 r)
      (error-errno "rmdir"))
    r))

;;  Select

(defcstruct fd-set
  (fds-bits fd-mask :count #.(floor (/ +fd-setsize+ +nfdbits+))))

(defun fd-elt (fd)
  (floor (/ fd +nfdbits+)))

(defun fd-mask (fd)
  (ash 1 (mod fd +nfdbits+)))

(defmacro fds-bits (fd fd-set)
  `(mem-aref (foreign-slot-value ,fd-set '(:struct fd-set)
                                 'fds-bits)
             'fd-mask (fd-elt ,fd)))

(defun fd-clr (fd fd-set)
  (setf (fds-bits fd fd-set)
        (logand (fds-bits fd fd-set) (lognot (fd-mask fd)))))

(defun fd-isset (fd fd-set)
  (not (zerop (logand (fds-bits fd fd-set) (fd-mask fd)))))

(defun fd-set (fd fd-set)
  (setf (fds-bits fd fd-set)
        (logior (fds-bits fd fd-set) (fd-mask fd))))

(defun fd-zero (fd-set)
  (dotimes (i (floor (/ (foreign-type-size '(:struct fd-set))
                        (foreign-type-size 'fd-mask))))
    (setf (mem-aref (foreign-slot-value fd-set '(:struct fd-set) 'fds-bits)
                    'fd-mask i)
          0)))

(defcstruct timeval
  (tv-sec :long)
  (tv-usec :long))

(defcfun ("select" c-select) :int
  (nfds :int)
  (readfds (:pointer (:struct fd-set)))
  (writefds (:pointer (:struct fd-set)))
  (exceptfds (:pointer (:struct fd-set)))
  (timeout (:pointer (:struct timeval))))

(defun select (nfds readfds writefds exceptfds timeout)
  (let ((r (c-select nfds readfds writefds exceptfds timeout)))
    (when (< r 0)
      (error-errno "select"))
    r))

(defun list-to-fd-set (list fd-set)
  (fd-zero fd-set)
  (dolist (fd list)
    (fd-set fd fd-set)))

(defun seconds-to-timeval (seconds tv)
  (let ((sec (floor seconds)))
    (setf (foreign-slot-value tv '(:struct timeval) 'tv-sec)
          sec
          (foreign-slot-value tv '(:struct timeval) 'tv-usec)
          (floor (* (- seconds sec) 1000000)))))

(defun fd-set-filter (list fd-set)
  (let ((result ()))
    (dolist (fd list)
      (when (fd-isset fd fd-set)
        (push fd result)))
    result))

(defmacro with-selected ((readfds &optional writefds exceptfds timeout)
                         (readfds-var writefds-var exceptfds-var)
                         &body body)
  (let ((g-readfds (gensym "R-"))
        (g-writefds (gensym "W-"))
        (g-exceptfds (gensym "E-"))
        (nfds (gensym "N-"))
        (fd-set-read (gensym "FDS-R-"))
        (fd-set-write (gensym "FDS-W-"))
        (fd-set-except (gensym "FDS-E-"))
        (tv (gensym "TV-")))
    `(let ((,g-readfds ,readfds)
           (,g-writefds ,writefds)
           (,g-exceptfds ,exceptfds)
           (,nfds 0))
       (cffi:with-foreign-objects ((,fd-set-read '(:struct fd-set))
                                   (,fd-set-write '(:struct fd-set))
                                   (,fd-set-except '(:struct fd-set))
                                   (,tv '(:struct timeval)))
         (loop
            (list-to-fd-set ,g-readfds ,fd-set-read)
            (list-to-fd-set ,g-writefds ,fd-set-write)
            (list-to-fd-set ,g-exceptfds ,fd-set-except)
            (when ,timeout
              (seconds-to-timeval ,timeout ,tv))
            (dolist (fd ,g-readfds)
              (when (< ,nfds fd) (setq ,nfds fd)))
            (dolist (fd ,g-writefds)
              (when (< ,nfds fd) (setq ,nfds fd)))
            (dolist (fd ,g-exceptfds)
              (when (< ,nfds fd) (setq ,nfds fd)))
            (incf ,nfds)
            (handler-case
                (select ,nfds ,fd-set-read ,fd-set-write ,fd-set-except
                        (if ,timeout ,tv (null-pointer)))
              (errno:errno-error (condition)
                (when (= errno:+eintr+ (errno:errno-error-errno condition))
                  (continue))))
            (let ((,readfds-var (fd-set-filter ,g-readfds ,fd-set-read))
                  (,writefds-var (fd-set-filter ,g-writefds ,fd-set-write))
                  (,exceptfds-var (fd-set-filter ,g-exceptfds ,fd-set-except)))
              (declare (ignorable ,readfds-var ,writefds-var ,exceptfds-var))
              (return (progn ,@body))))))))
