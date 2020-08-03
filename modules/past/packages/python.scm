;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2020 Bonface Munyoki <bonfacemunyoki@gmail.com>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
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
      (patches
       (search-patches
        "past/patches/python24-get-platform.patch"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          ;; Link Expat instead of embedding the bundled one.
          (delete-file-recursively "Modules/expat")
          (substitute* "Modules/Setup.dist"
            (("^#pyexpat.*") "pyexpat pyexpat.c -lexpat\n"))
          #t))
      (sha256
       (base32
        "021y88a4ki07dgq19yhg6zfvmncfiz7h5b2255438i9zmlwl246s"))))
    (outputs '("out"))
    (arguments
     `(#:parallel-tests? #f
       ,@(substitute-keyword-arguments (package-arguments python-2)
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
                              zlib zlibmodule.c -I~a/include -L~a/lib -lz~@
                              *static*~@
                              cPickle cPickle.c~@
                              array arraymodule.c~%"
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
                  '("test_anydbm.py"
                    "test_socket.py"
                    "test_whichdb.py"
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
        ((#:make-flags _) ''()))))
    (native-search-paths
      (list (search-path-specification
              (variable "PYTHONPATH")
              (files '("lib/python2.4/site-packages")))))
    (inputs
     `(("libnsl" ,libnsl)
       ("libtirpc" ,libtirpc)
       ("openssl" ,openssl-1.0)
       ,@(alist-delete "openssl" (package-inputs python-2))))
    (native-inputs
     ;; This fixes some serious math bugs in the compiled program
     `(("gcc" ,gcc-5)
       ,@(package-native-inputs python-2)))
    (properties '((release-date "2008-12-19")))
    (home-page "https://www.python.org/downloads/release/python-246/")
    (synopsis "Python 2.4.6, released 2008-12-19")
    (description
     "The last bugfix release of the Python 2.4 series, which
started with 2.4.0, released on 2004-11-30.  Python 2.5 was
released on 2006-09-19.")))



(define-public python24-argparse
  (package
    (name "python24-argparse")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "argparse" version))
        (sha256
         (base32
          "1r6nznp64j68ih1k537wms7h57nvppq0szmwsaf99n71bfjqkc32"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (when tests?
               ;; Taken from tox.ini
               (invoke "python" "test/test_argparse.py"))
             #t)))))
    (native-inputs
     `(("setuptools" ,python24-setuptools)))
    (home-page "https://github.com/ThomasWaldmann/argparse/")
    (properties '((release-date "2015-09-12")))
    (synopsis "Python command-line parsing library")
    (description
     "This package is mostly for people who want to have @code{argparse} on
older Pythons because it was not part of the standard library back then.")
    (license license:psfl)))

(define-public python24-dateutil
  (package
    (name "python24-dateutil")
    (version "2.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-dateutil" version))
        (sha256
         (base32
          "1vlx0lpsxjxz64pz87csx800cwfqznjyr2y7nk3vhmzhkwzyqi2c"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (when tests?
               ;; Taken from tox.ini
               (invoke "python" "test.py"))
             #t)))))
    (native-inputs
     `(("setuptools" ,python24-setuptools)))
    (propagated-inputs
     `(("six" ,python24-six)))
    (home-page "https://dateutil.readthedocs.io/en/stable/")
    (properties '((release-date "2012-03-28")))
    (synopsis "python-dateutil 2.1, released 2012-03-28")
    (description "The dateutil module provides powerful extensions to the
standard datetime module, available in Python 2.3+.")
    (license license:bsd-3)))

(define-public python24-nose
  (package
    (name "python24-nose")
    (version "0.10.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/nose-devs/nose")
              (commit (string-append "tag/" version "-release"))))
       (file-name (git-file-name "nose" version))
       (sha256
        (base32
         "1ajx6m14wav0xq1cba35sqv9b6fv521difkgighxr4mfpkl2vybp"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-man-directory
           (lambda _
             (substitute* "setup.py"
               (("man/man1") "share/man/man1"))
             #t))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             ;; This test fails, funtional_tests/support/empty isn't created.
             (delete-file "functional_tests/test_success.py")
             (when tests?
               (invoke "python" "selftest.py"))
             #t)))))
    (home-page "http://readthedocs.org/docs/nose/")
    (properties '((release-date "2008-10-03")))
    (synopsis "Nose 0.10.4, released 2008-10-03")
    (description
     "Nose extends the unittest library to make testing easier.")
    (license license:lgpl2.0+)))

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

(define-public python24-numpy-1.0
  (package
    (name "python24-numpy")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/numpy/numpy")
              (commit (string-append "v" version))))
       (file-name (git-file-name "numpy" version))
       (sha256
        (base32
         "0648id8jfjscv7y672ppl72nsywpi8i426498c76a087w8749xy1"))))
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
    (properties '((release-date "2007-11-08")))
    (synopsis "NumPy 1.0.4, released on 2007-11-08")
    (home-page "https://numpy.org")
    (description "NumPy is the fundamental package for scientific computing
with Python.  It contains among other things: a powerful N-dimensional array
object, sophisticated (broadcasting) functions, tools for integrating C/C++
and Fortran code, useful linear algebra, Fourier transform, and random number
capabilities.")
    (license license:bsd-3)))

(define-public python24-numpy-1.1
  (package
    (inherit python24-numpy-1.0)
    (name "python24-numpy")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/numpy/numpy")
              (commit (string-append "v" version))))
       (file-name (git-file-name "numpy" version))
       (sha256
        (base32
         "04dkq22yyl8ap4b5mmgalnp9wrs5pdi5j9wwkv2pabnljfrwikiy"))))
    (properties '((release-date "2008-07-31")))
    (synopsis "NumPy 1.1.1, released on 2008-07-31")))

(define-public python24-numpy-1.2
  (package
    (inherit python24-numpy-1.1)
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/numpy/numpy")
              (commit (string-append "v" version))))
       (file-name (git-file-name "numpy" version))
       (sha256
        (base32
         "14p9qnmqx7mw4vgdz2p5jsghf3dj8i4ln8fhc8l4dc9sxyhqq739"))))
    (native-inputs
     `(("nose" ,python24-nose)))
    (properties '((release-date "2008-10-28")))
    (synopsis "NumPy 1.2.1, released on 2008-10-28")))

(define-public python24-pyx
  (package
    (name "python24-pyx")
    (version "0.12.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/pyx-project/pyx")
               (commit version)))
        (file-name (git-file-name "PyX" version))
        (sha256
         (base32
          "0jzb21z3ypwbz967cssm9gbb8psl92x6x9qajsbhkvmqa7xa8ibs"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; The tests in test/functional cannot find the font files.
             ;(invoke "make" "-C" "test")
             (with-directory-excursion "test/unit"
               (invoke "python" "test.py")))))))
    (native-inputs
     `(("texlive" ,(texlive-union (list texlive-bin
                                        texlive-fonts-cm
                                        texlive-latex-base)))))
    (properties '((release-date "2012-10-26")))
    (home-page "https://pyx-project.org/")
    (synopsis "Create PostScript, PDF, and SVG files")
    (description "Pyx is a Python package for the generation of PostScript, PDF,
and SVG files.  It combines an abstraction of the PostScript drawing model with
a TeX/LaTeX interface.  Complex tasks like 2d and 3d plots in publication-ready
quality are built out of these primitives.")
    (license license:gpl2+)))

(define-public python24-setuptools
  (package
    (inherit python-setuptools)
    (name "python24-setuptools")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setuptools" version))
       (sha256
        (base32
         "1gfvalhvzcskwj85r3lh9sx190f8k807vz5zln8agaw31ak8cf96"))))
    (arguments
     `(#:python ,python-2.4
       #:tests? #f))    ; Tests want SVN and internet access.
    (properties '((release-date "2013-12-01")))
    (synopsis "Setuptools 1.4.2, released on 2013-13-01")))

(define-public python24-matplotlib
  (package
    (name "python24-matplotlib")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "matplotlib" version))
       (sha256
        (base32
         "1whjqg1dhlsah0sfaf3nfs0a8d2m4sk6h0g3xd356i6903sfh932"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (delete-file-recursively "lib/dateutil")
           (delete-file-recursively "lib/pytz")
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("setuptools" ,python24-setuptools)))
    (propagated-inputs
     `(("dateutil" ,python24-dateutil)
       ("numpy" ,python24-numpy-1.1)
       ("pytz" ,python24-pytz)))
    (inputs
     `(("freetype" ,freetype)
       ("libpng" ,libpng)))
    (properties '((release-date "2011-11-14")))
    (home-page "https://matplotlib.org/")
    (synopsis "Matplotlib 1.1.0 released on 2011-11-14")
    (description "Matplotlib is a comprehensive library for creating static,
animated, and interactive visualizations in Python.")
    (license license:psfl)))

(define-public python24-py
  (package
    (name "python24-py")
    (version "1.4.31")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "py" version))
        (sha256
         (base32
          "0561gz2w3i825gyl42mcq14y3dcgkapfiv5zv9a2bz15qxiijl56"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:tests? #f))    ; Tests rely on pytest
    (native-inputs
     `(("setuptools" ,python24-setuptools)))
    (home-page "https://py.readthedocs.io/en/latest/")
    (properties '((release-date "2015-11-27")))
    (synopsis "Py 1.4.31, released 2015-11-27")
    (description
     "Py is a Python library for file name parsing, @code{.ini} file parsing,
I/O, code introspection, and logging.")
    (license license:expat)))

(define-public python24-pytest
  (package
    (name "python24-pytest")
    (version "2.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest" version))
        (sha256
         (base32
          "1q00r8jwvrdpx0fhjsbp6g61qxfbmdxc4n5rzf0m7sdi6babxjxw"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
               (when tests?
                 ;; Taken from tox.ini
                 (with-directory-excursion "testing"
                   (invoke "python" "-m" "pytest" "--lsof" "-rfsxX")))
               #t)))))
    (propagated-inputs
     `(("argparse" ,python24-argparse)  ; python < 2.7
       ("py" ,python24-py)))
    (native-inputs
     `(("setuptools" ,python24-setuptools)))
    (home-page "https://docs.pytest.org/en/stable/")
    (properties '((release-date "2013-10-04")))
    (synopsis "Py.test 2.4.2, released 2013-10-04")
    (description "Pytest is a testing tool that provides auto-discovery of test
modules and functions, detailed info on failing assert statements, modular
fixtures, and many external plugins.")
    (license license:expat)))

(define-public python24-pytz
  (package
    (inherit python-pytz)
    (name "python24-pytz")
    (arguments
     `(#:python ,python-2.4))
    (native-inputs
     `(("setuptools" ,python24-setuptools)))))

(define-public python24-six
  (package
    (name "python24-six")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "six" version))
        (sha256
         (base32
          "0rw3bymdjs1fvs8myjqx4rqhxpn4mklprjqs8705qxgvvzbayigh"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (when tests?
               ;; Taken from tox.ini
               (invoke "py.test" "-rfsxX")
             #t))))))
    (home-page "https://pypi.org/project/six/")
    (native-inputs
     `(("pytest" ,python24-pytest)
       ("setuptools" ,python24-setuptools)))
    (properties '((release-date "2013-09-02")))
    (synopsis "Six 1.4.1, released 2013-09-02")
    (description
     "Six is a Python 2 and 3 compatibility library.  It provides utility
functions for smoothing over the differences between the Python versions with
the goal of writing Python code that is compatible on both Python versions.")
    (license license:expat)))
