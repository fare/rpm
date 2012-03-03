;;; -*- Lisp -*-

(defsystem :rpm
  :depends-on (:inferior-shell)
  :components
  ((:file "pkgdcl")
   (:file "version" :depends-on ("pkgdcl"))
   (:file "upgrade" :depends-on ("pkgdcl"))))
