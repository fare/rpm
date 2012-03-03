#+xcvb (module ())

(in-package :cl)

(defpackage :rpm
  (:use :inferior-shell)
  (:export
   #:parse-rpm-versioned-name
   #:rpm-versioned-name-basename #:rpm-versioned-name-version
   #:parse-rpm-pathname
   #:rpm-pathname-packagename #:rpm-pathname-version
   #:rpm-version=
   #:rpm-version<= #:rpm-version< #:rpm-version>= #:rpm-version>
   #:rpms-installed #:rpms-to-update))
