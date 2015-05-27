;;; -*- Lisp -*-

(defsystem "rpm"
  :version "0.0.1"
  :description "functions to use the RedHat Package Management system"
  :author "Francois-Rene Rideau"
  :license "MIT"
  :depends-on ("inferior-shell" "lambda-reader" "cl-ppcre" "fare-utils")
  :components
  ((:file "pkgdcl")
   (:file "specials" :depends-on ("pkgdcl"))
   (:file "character-classes" :depends-on ("pkgdcl"))
   (:file "version" :depends-on ("character-classes"))
   (:file "upgrade" :depends-on ("pkgdcl"))))
