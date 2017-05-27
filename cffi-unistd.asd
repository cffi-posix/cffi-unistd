
(in-package :common-lisp-user)

(defpackage :cffi-unistd.system
  (:use :common-lisp :asdf))

(in-package :cffi-unistd.system)

(defsystem :cffi-unistd
  :name "cffi-unistd"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.1"
  :description "Common Lisp wrapper for unistd.h"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "cffi-errno")
  :components
  ((:file "package")
   (:cffi-grovel-file "grovel-unistd" :depends-on ("package"))
   (:file "cffi-unistd" :depends-on ("grovel-unistd"))))
