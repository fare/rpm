#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :rpm)

(defun hash-rpm-versioned-names-by-name (rpm-list)
  (loop :with hash = (make-hash-table :test 'equal)
    :for rpm :in rpm-list
    :do (setf (gethash (parse-rpm-versioned-name rpm) hash) rpm)
    :finally (return hash)))

(defun hash-rpm-pathnames-by-packagename (rpm-list)
  (loop :with hash = (make-hash-table :test 'equal)
    :for rpm :in rpm-list
    :do (setf (gethash (rpm-pathname-packagename rpm) hash) rpm)
    :finally (return hash)))

(defun rpms-installed (&key (packagenames t) host)
  (run/lines
   `(pipe (rpm -qa)
          ,@(unless (eq packagenames t)
              `((egrep ("^(" ,@(loop :for (name . more) :on packagenames
                                 :collect name :when more :collect "|")
                             ")-[^-]+-[^-]+$")))))
   :host host))

(defun rpms-to-update (desired-rpms &key
                       host (test 'rpm-version<=))
  (loop
    :with packagenames = (mapcar 'rpm-pathname-packagename desired-rpms)
    :with installed-rpms = (rpms-installed :packagenames packagenames :host host)
    :with hash = (hash-rpm-versioned-names-by-name installed-rpms)
    :for desired-rpm :in desired-rpms
    :for name = (rpm-pathname-packagename desired-rpm)
    :for installed-rpm = (gethash name hash)
    :for desired-version = (rpm-pathname-version desired-rpm)
    :for installed-version = (when installed-rpm
                               (rpm-versioned-name-version installed-rpm))
    :unless (funcall test desired-version installed-version)
    :collect desired-rpm))
