#+xcvb (module (:depends-on ("character-classes")))

(in-package :qwalitee)

(named-readtables:in-readtable :λ-standard)

(defun valid-rpm-version-component-p (string &key start end)
  (and
   (find-if #'ascii-digit-p string :start start :end end)
   (not (find-if (λ (x) (find x "-~/")) string :start start :end end))
   (not (search ".." string :start2 start :end2 end))))

(defun valid-rpm-architecture-component-p (string &key start end)
  (and
   (find-if #'ascii-letter-p string :start start :end end)
   (not (find-if-not 'ascii-alphanumeric-or-underscore-p
                     string :start start :end end))))

(defun parse-rpm-versioned-name (string &key (start 0) (end (length string)))
  (flet ((err () (error "No valid RPM version in package name ~S" string))
         (split-at (n) (values (subseq string start n)
                               (subseq string (1+ n) end))))
    (let ((pos (position #\- string :from-end t :start start :end end)))
      (unless (and pos (valid-rpm-version-component-p
                        string :start (1+ pos) :end end))
        (err))
      (let ((pos2 (position #\- string :from-end t :start start :end pos)))
        (split-at (if (and pos2
                           (valid-rpm-version-component-p
                            string :start (1+ pos2) :end pos))
                      pos2 pos))))))

(defun rpm-versioned-name-basename (string)
  (nth-value 0 (parse-rpm-versioned-name string)))

(defun rpm-versioned-name-version (string)
  (nth-value 1 (parse-rpm-versioned-name string)))

(defun parse-rpm-pathname (pathname)
  (with-nesting ()
    (let* ((pathname (pathname pathname))
           (directory (pathname-directory-pathname pathname))
           (type (pathname-type pathname))
           (basename (pathname-name pathname))
           (dotpos (position #\. basename :from-end t))))
    (progn
      (assert (equal type "rpm"))
      (assert dotpos)
      (assert (valid-rpm-architecture-component-p basename :start (1+ dotpos))))
    (let ((architecture (subseq basename (1+ dotpos)))))
    (multiple-value-bind (name version)
        (parse-rpm-versioned-name basename :end dotpos))
    (values directory name version architecture)))

(defun rpm-pathname-packagename (pathname)
  (nth-value 1 (parse-rpm-pathname pathname)))

(defun rpm-pathname-version (pathname)
  (nth-value 2 (parse-rpm-pathname pathname)))

;; For version comparison, I followed
;; https://twiki.cern.ch/twiki/bin/view/Main/RPMAndDebVersioning

(defun parse-rpm-version-component (v)
  "Given a version or release component of a RPM, parse it into a list
of numbers and letters, e.g. \"0.99p7\" => (0 99 \"p\" 7)"
  (loop :with r = () :with l = () :with len = (length v) :with i = 0
    :while (< i len) :do
    (flet ((handle-component (predicate push)
             (when (and (< i len) (funcall predicate (char v i)))
               (let ((j (or (position-if-not predicate v :start (1+ i)) len)))
                 (when push (push (funcall push (subseq v i j)) l))
                 (setf i j)))))
      (handle-component #'ascii-letter-p #'parse-integer)
      (handle-component #'ascii-digit-p #'identity)
      (handle-component #'ascii-non-alphanumeric-p nil))
    :finally (return (reverse l))))

(defun compare-rpm-version-chunks (ch1 ch2)
  "Given the first chunks of two respective version numbers,
return the symbol < = > depending on which of predicates hold,
or nil is none does"
  (check-type ch1 (or integer string))
  (check-type ch2 (or integer string))
  (cond
    ((and (integerp ch1) (integerp ch2))
     (cond
       ((< ch1 ch2) '<)
       ((> ch1 ch2) '>)
       (t '=)))
    ;; RPM: integer block beats alphanumeric, so 1.4.1 > 1.4p8
    ((integerp ch1)
     '>)
    ((integerp ch2)
     '<)
    (t
     (cond
       ((string< ch1 ch2) '<)
       ((string> ch1 ch2) '>)
       (t '=)))))

(defun compare-rpm-version-components (v1 v2)
  (let ((l1 (parse-rpm-version-component v1))
        (l2 (parse-rpm-version-component v2)))
    (loop :with l1 = (parse-rpm-version-component v1)
      :with l2 = (parse-rpm-version-component v2)
      :while (and l1 l2) :do
      (let ((r (compare-rpm-version-chunks (pop l1) (pop l2))))
        (ecase r
          ((< > nil) (return r))
          ((=) nil)))
      :finally
      (cond
        (l1 (return '>))
        (l2 (return '<))
        (t (return '=))))))

(defun parse-rpm-version (x)
  (block nil
    (cl-ppcre:register-groups-bind (epoch version release)
        ("^(?:([0-9]+):)?([^-/~]+)(?:-([^-/~]+))?$" x) ;; also .. forbidden
      (return (values (if (emptyp epoch) 0 (parse-integer epoch))
                      version release)))
    (error "bad rpm version ~S" x)))

(defun compare-rpm-versions (v1 v2)
  (multiple-value-bind (epoch1 version1 release1)
      (parse-rpm-version v1)
    (multiple-value-bind (epoch2 version2 release2)
        (parse-rpm-version v2)
      (cond
        ((> epoch1 epoch2)
         '>)
        ((< epoch1 epoch2)
         '<)
        (t
         (let ((r (compare-rpm-version-components version1 version2)))
           (ecase r
             ((< > nil) r)
             ((=) (compare-rpm-version-components release1 release2)))))))))

(defun rpm-version<= (v1 v2)
  (ecase (compare-rpm-versions v1 v2)
    ((< =) t)
    ((>) nil)))

(defun rpm-version>= (v1 v2)
  (ecase (compare-rpm-versions v1 v2)
    ((> =) t)
    ((<) nil)))

(defun rpm-version< (v1 v2)
  (ecase (compare-rpm-versions v1 v2)
    ((<) t)
    ((> =) nil)))

(defun rpm-version> (v1 v2)
  (ecase (compare-rpm-versions v1 v2)
    ((>) t)
    ((< =) nil)))

(defun rpm-version= (v1 v2)
  (ecase (compare-rpm-versions v1 v2)
    ((=) t)
    ((< >) nil)))
