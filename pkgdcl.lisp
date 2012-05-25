#+xcvb (module ())

(in-package :cl)

(defpackage :rpm
  (:use :common-lisp :inferior-shell :λ-reader :fare-utils :xcvb-driver)
  (:import-from :asdf
   #:probe-file*)
  (:export
   #:parse-rpm-versioned-name
   #:rpm-versioned-name-basename #:rpm-versioned-name-version
   #:hash-rpm-versioned-names-by-name
   #:hash-rpm-pathnames-by-packagename
   #:parse-rpm-version #:parse-rpm-pathname
   #:rpm-pathname-packagename #:rpm-pathname-version
   #:rpm-version=
   #:rpm-version<= #:rpm-version< #:rpm-version>= #:rpm-version>
   #:rpms-installed #:rpms-to-update))
