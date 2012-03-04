#+xcvb (module (:depends-on ("pkgdcl")))

(in-package :rpm)

(defun asdf-to-rpm-architecture (x)
  (case x
    ((:x64 :amd64) "x86_64")
    ((:x86) "i686") ;; older "i386", "i586" also exist, and "i486", "athlon"
    ((:arm) "armv4l") ;; older armv3l armv4b
    ((:alpha) "alpha")
    ;; These I'm not sure about...
    ((:sparc64) "sparc64") ((:sparc32) "sparc32") ; also "sparcv9" and more
    ((:ppc64) "ppc64") ((:ppc32) "ppc") ; also "ppciseries" and more
    ((:mipsel) "mispel") ((:mipseb :mips) "mips") ; also "sgi"
    ((:hppa64) "hppa2.0") ((:hppa) "hppa1.1") ; also "hppa1.2", "hppa1.0", "parisc"
    ;; no RPM that I know of on :imach or :java :-)
    ;; and no supported Lisp that I know of on "ia64" "s390x" "m68k"
    ))

(defparameter *rpm-architecture*
  (asdf-to-rpm-architecture (asdf::architecture)))
