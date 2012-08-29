;;; -*- Lisp -*-

(defsystem :rpm
  :depends-on (:inferior-shell :lambda-reader :cl-ppcre :xcvb-utils)
  :components
  ((:file "pkgdcl")
   (:file "specials" :depends-on ("pkgdcl"))
   (:file "character-classes" :depends-on ("pkgdcl"))
   (:file "version" :depends-on ("character-classes"))
   (:file "upgrade" :depends-on ("pkgdcl"))))
