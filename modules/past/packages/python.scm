;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;;
;;; This file is part of Guix Past.
;;;
;;; Guix Past is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; Guix Past is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Guix Past.  If not, see <http://www.gnu.org/licenses/>.

(define-module (past packages python)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (srfi srfi-1))

(define-public python-2.4                           ; Dec. 2008
  (package
    (inherit python-2)
    (version "2.4.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.python.org/ftp/python/"
                          version "/Python-" version ".tar.bz2"))
      (sha256
       (base32
        "021y88a4ki07dgq19yhg6zfvmncfiz7h5b2255438i9zmlwl246s"))))
    (outputs '("out"))
    (arguments
      (substitute-keyword-arguments (package-arguments python-2)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-after 'unpack 'create-setup-local
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((zlib (assoc-ref inputs "zlib"))
                      (tcl  (assoc-ref inputs "tcl"))
                      (tk   (assoc-ref inputs "tk"))
                      (gdbm (assoc-ref inputs "gdbm"))
                      (read (assoc-ref inputs "readline"))
                      (rpc  (assoc-ref inputs "libtirpc"))
                      (nsl  (assoc-ref inputs "libnsl"))
                      (ssl  (assoc-ref inputs "openssl")))
                  (with-output-to-file "Modules/Setup.local"
                    (lambda _
                      (format #t "readline readline.c -I~a/include -L~a/lib -lreadline~@
                              _ssl _ssl.c -DUSE_SSL -I$~a/include/openssl -L~a/lib -lssl -lcrypto~@
                              _tkinter _tkinter.c tkappinit.c -DWITH_APPINIT -L~a/lib -I~a/include -L~a/lib -I~a/include -ltk~a -ltcl~a~@
                              gdbm gdbmmodule.c -I~a/include -L~a/lib -lgdbm~@
                              nis nismodule.c -I~a/include/tirpc -I~a/include -ltirpc -lnsl~@
                              zlib zlibmodule.c -I~a/include -L~a/lib -lz~%"
read read ssl ssl tcl tcl tk tk ,(version-major+minor (package-version tcl)) ,(version-major+minor (package-version tcl)) gdbm gdbm rpc nsl zlib zlib))))
                  #t))
              (add-after 'unpack 'patch-rpc-location
                (lambda _
                  (substitute* "Modules/nismodule.c"
                    (("<rpc/") "<tirpc/rpc/"))
                  (substitute* "setup.py"
                    (("\\['nsl'") "['nsl', 'tirpc'"))
                  #t))
              (add-after 'unpack 'skip-crypt-module
                (lambda _
                  (substitute* "setup.py"
                    ((".*cryptmodule.c.*") "\n"))
                  #t))
            (add-before 'check 'delete-failing-tests
              (lambda _
                (for-each
                  (lambda (file)
                    (delete-file (string-append "Lib/test/" file)))
                  '("test_anydbm.py" "test_array.py" "test_decimal.py"
                    "test_getargs2.py" "test_long.py" "test_math.py"
                    "test_mhlib.py" "test_random.py" "test_socket.py"
                    "test_str.py" "test_userstring.py" "test_whichdb.py"
                    "test_zlib.py"))
                #t))
            (add-after 'check 'find-netinet-in-h
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((glibc (assoc-ref inputs "libc")))
                  (substitute* (find-files "Lib/plat-generic" ".*")
                    (("/usr/include/netinet/in.h")
                     (string-append glibc "/include/netinet/in.h")))
                  #t)))
            (delete 'move-tk-inter)))
         ;; Python-2.4 does not support '-j'.
         ((#:make-flags _) ''())))
    (native-search-paths
      (list (search-path-specification
              (variable "PYTHONPATH")
              (files '("lib/python2.4/site-packages")))))
    (inputs
     `(("libnsl" ,libnsl)
       ("libtirpc" ,libtirpc)
       ("openssl" ,openssl-1.0)
       ,@(alist-delete "openssl" (package-inputs python-2))))
    (properties '((release-date "2008-12-19")))
    (home-page "https://www.python.org/downloads/release/python-246/")
    (synopsis "Python 2.4.6, released 2008-12-19")
    (description
     "The last bugfix release of the Python 2.4 series, which
started with 2.4.0, released on 2004-11-30.  Python 2.5 was
released on 2006-09-19.")))

(define-public python24-numarray
  (package
    (name "python24-numarray")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/numpy/Old Numarray/" version
             "/numarray-" version ".tar.gz"))
       (sha256
        (base32
         "0x1i4j7yni7k4p9kjxs1lgln1psdmyrz65wp2yr35yn292iw2vbg"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (invoke "python" "setup.py" "config" "build"
                     "--gencode" "--use_lapack")))
         (add-after 'unpack 'find-lapack-and-openblas
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((lapack (assoc-ref inputs "lapack"))
                   (blas   (assoc-ref inputs "openblas")))
               (substitute* "cfg_packages.py"
                 (("lapack_libs = .*'m']")
                  "lapack_libs = ['lapack', 'openblas', 'm']\n")
                 (("lapack_dirs = .*")
                  (string-append "lapack_dirs = ['"
                                 lapack "/lib', '" blas "/lib']\n"))
                 (("lapack_include_dirs = .*")
                  (string-append "lapack_include_dirs = ['"
                                 lapack "/include', '" blas "/include']\n")))
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "python" "setup.py" "config"
                       "install" "--use_lapack"
                       (string-append "--prefix=" out))))))
       #:tests? #f))   ; no test target
    (inputs
     `(("lapack" ,lapack)
       ("openblas" ,openblas)))
    (properties '((release-date "2006-08-26")))
    (home-page "https://sourceforge.net/projects/numpy/files/Old%20Numarray/1.5.2/")
    (synopsis "Final numarray release from 2006-08-26")
    (description "Numarray and Numeric were the predecessors of NumPy.
Numarray was created as an alternative to Numeric because the latter
was cumbersome to use when efficiency for large array operations was a
priority.  Howver, numarray was less efficient for small arrays, and
thus could not replace Numeric.  Many packages of the early SciPy
ecosystem supported both Numeric and numarray, with the choice made
at build time.")
    (license license:bsd-3)))

(define-public python24-numpy
  (package
    (name "python24-numpy")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/numpy/numpy")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "04dkq22yyl8ap4b5mmgalnp9wrs5pdi5j9wwkv2pabnljfrwikiy"))))
    (build-system python-build-system)
    (inputs
     `(("lapack" ,lapack)
       ("openblas" ,openblas)))
    (arguments
     `(#:python ,python-2.4
       #:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((lapack (assoc-ref inputs "lapack"))
                   (openblas (assoc-ref inputs "openblas")))
               (with-output-to-file "site.cfg"
                 (lambda _
                   (format #t "[DEFAULT]
library_dirs = ~a/lib:~a/lib
include:dirs = ~a/include:~a/include~%"
lapack openblas lapack openblas))))
               #t))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (begin
                 ;; Taken from test.sh
                 (with-directory-excursion "/tmp"
                   (add-installed-pythonpath inputs outputs)
                   (invoke "python" "-c"
                           "import numpy; print numpy; numpy.test(level = 9999); numpy.show_config()"))))
             #t)))))
    (home-page "https://numpy.org")
    (synopsis "NumPy 1.1.1, released on 2008-07-31")
    (description "NumPy is the fundamental package for scientific computing
with Python.  It contains among other things: a powerful N-dimensional array
object, sophisticated (broadcasting) functions, tools for integrating C/C++
and Fortran code, useful linear algebra, Fourier transform, and random number
capabilities.")
    (properties '((release-date "2008-07-31")))
    (license license:bsd-3)))
