
(in-package :common-lisp-user)

(defpackage :cffi-unistd.system
  (:use :common-lisp :asdf))

(in-package :cffi-unistd.system)

(defsystem "cffi-unistd"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "cffi-errno")
  :components
  ((:file "package")
   (:cffi-grovel-file "grovel-unistd" :depends-on ("package"))
   (:file "cffi-unistd" :depends-on ("grovel-unistd"))))
