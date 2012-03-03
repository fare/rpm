;;; -*- Lisp -*-

(defsystem :rpm
  :depends-on (:inferior-shell :lambda-reader :fare-utils)
  :components
  ((:file "pkgdcl")
   (:file "character-classes" :depends-on ("pkgdcl"))
   (:file "version" :depends-on ("character-classes"))
   (:file "upgrade" :depends-on ("pkgdcl"))))
