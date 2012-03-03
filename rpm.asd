;;; -*- Lisp -*-

(defsystem :rpm
  :depends-on (:inferior-shell)
  :components
  ((:file "pkgdcl")
   (:file "version" :depends-on ("pkgdcl"))
   (:file "rpm" :depends-on ("pkgdcl"))))
