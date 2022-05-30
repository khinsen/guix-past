;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2019, 2020, 2022 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (past packages statistics)
  #:use-module (past packages tls)
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
            (("^#pyexpat.*") "pyexpat pyexpat.c -lexpat\n"))))
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
                              zlib zlibmodule.c -I~a/include -L~a/lib -lz~%"
read read ssl ssl tcl tcl tk tk ,(version-major+minor (package-version tcl)) ,(version-major+minor (package-version tcl)) gdbm gdbm rpc nsl zlib zlib))))))
            (add-after 'unpack 'patch-rpc-location
              (lambda _
                (substitute* "Modules/nismodule.c"
                  (("<rpc/") "<tirpc/rpc/"))
                (substitute* "setup.py"
                  (("\\['nsl'") "['nsl', 'tirpc'"))))
            (add-after 'unpack 'skip-crypt-module
              (lambda _
                (substitute* "setup.py"
                  ((".*cryptmodule.c.*") "\n"))))
            (add-before 'check 'delete-failing-tests
              (lambda _
                (with-directory-excursion "Lib/test/"
                  (for-each delete-file
                            '("test_anydbm.py"
                              "test_mhlib.py"
                              "test_socket.py"
                              "test_whichdb.py"
                              "test_zlib.py")))))
            (add-after 'check 'find-netinet-in-h
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* (find-files "Lib/plat-generic")
                  (("/usr/include/netinet/in.h")
                   (search-input-file inputs "/include/netinet/in.h")))))
            (replace 'remove-tests
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out     (assoc-ref outputs "out"))
                       (libdir  (string-append out "/lib/python2.4"))
                       (testdir (string-append libdir "/test")))
                  (with-directory-excursion testdir
                    (for-each delete-file-recursively
                              (scandir testdir
                                       (match-lambda
                                         ((or "." "..") #f)
                                         ("support" #f)
                                         (file
                                          (not
                                           (string-prefix? "test_support."
                                                           file))))))
                    (call-with-output-file "__init__.py" (const #t)))
                  (for-each delete-file-recursively
                            (find-files libdir "^test(s)?$"
                                        #:directories? #t)))))
            (replace 'install-sitecustomize.py
              ;; TODO: Adjust sitecustomize.py for python-2.4.
              ;,(customize-site version))
              (lambda _ #t))
            ;; Some later phases depend on these phases existing.
            (replace 'move-tk-inter
              (lambda _ #t))
            (replace 'move-idle
              (lambda _ #t))))
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
    (properties '((release-date . "2008-12-19")))
    (home-page "https://www.python.org/downloads/release/python-246/")
    (synopsis "Python 2.4.6, released 2008-12-19")
    (description
     "The last bugfix release of the Python 2.4 series, which
started with 2.4.0, released on 2004-11-30.  Python 2.5 was
released on 2006-09-19.")))

(define-public python-3.8
  (package
    (name "python")
    (version "3.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.python.org/ftp/python/"
                                  version "/Python-" version ".tar.xz"))
              (patches (search-patches
                        "past/patches/python-3.8-arm-alignment.patch"
                        "past/patches/python-3.8-fix-tests.patch"
                        "past/patches/python-3.8-fix-tests2.patch"
                        "past/patches/python-3.8-deterministic-build-info.patch"
                        "past/patches/python-3.8-search-paths.patch"
                        "past/patches/python-3.8-hurd-configure.patch"))
              (sha256
               (base32
                "1c43dbv9lvlp3ynqmgdi4rh8q94swanhqarqrdx62zmigpakw073"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Delete the bundled copy of libexpat.
                  (delete-file-recursively "Modules/expat")
                  (substitute* "Modules/Setup"
                    ;; Link Expat instead of embedding the bundled one.
                    (("^#pyexpat.*") "pyexpat pyexpat.c -lexpat\n"))))))
    (outputs '("out"
               "tk"))                   ;tkinter; adds 50 MiB to the closure
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:configure-flags
       (list "--enable-shared"          ;allow embedding
             "--with-system-expat"      ;for XML support
             "--with-system-ffi"        ;build ctypes
             "--with-ensurepip=install" ;install pip and setuptools
             "--enable-unicode=ucs4"

             ;; Prevent the installed _sysconfigdata.py from retaining a reference
             ;; to coreutils.
             "INSTALL=install -c"
             "MKDIR_P=mkdir -p"

             ;; Disable runtime check failing if cross-compiling, see:
             ;; https://lists.yoctoproject.org/pipermail/poky/2013-June/008997.html
             ,@(if (%current-target-system)
                   '("ac_cv_buggy_getaddrinfo=no"
                     "ac_cv_file__dev_ptmx=no"
                     "ac_cv_file__dev_ptc=no")
                   '())
             (string-append "LDFLAGS=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       ;; With no -j argument tests use all available cpus, so provide one.
       #:make-flags
       (list (string-append
              (format #f "TESTOPTS=-j~d" (parallel-job-count))
              ;; test_mmap fails on low-memory systems.
              " --exclude test_mmap"
              ;; test_socket may hang and eventually run out of memory
              ;; on some systems: <https://bugs.python.org/issue34587>.
              " test_socket"
              ,@(if (hurd-target?)
                    '(" test_posix"     ;multiple errors
                      " test_time"
                      " test_pty"
                      " test_shutil"
                      " test_tempfile"  ;chflags: invalid argument:
                                        ;  tbv14c9t/dir0/dir0/dir0/test0.txt
                      " test_asyncio"   ;runs over 10min
                      " test_os"        ;stty: 'standard input':
                                        ;  Inappropriate ioctl for device
                      " test_openpty"   ;No such file or directory
                      " test_selectors" ;assertEqual(NUM_FDS // 2, len(fds))
                                        ;  32752 != 4
                      " test_compileall" ;multiple errors
                      " test_poll"       ;list index out of range
                      " test_subprocess" ;runs over 10min
                      " test_asyncore"   ;multiple errors
                      " test_threadsignals"
                      " test_eintr"     ;Process return code is -14
                      " test_io"        ;multiple errors
                      " test_logging"
                      " test_signal"
                      " test_threading" ;runs over 10min
                      " test_flags"     ;ERROR
                      " test_bidirectional_pty"
                      " test_create_unix_connection"
                      " test_unix_sock_client_ops"
                      " test_open_unix_connection"
                      " test_open_unix_connection_error"
                      " test_read_pty_output"
                      " test_write_pty")
                    '())))

       #:modules ((ice-9 ftw) (ice-9 match)
                  (guix build utils) (guix build gnu-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-lib-shells
           (lambda _
             ;; This variable is used in setup.py to enable cross compilation
             ;; specific switches. As it is not set properly by configure
             ;; script, set it manually.
             ,@(if (%current-target-system)
                   '((setenv "_PYTHON_HOST_PLATFORM" ""))
                   '())
             ;; Filter for existing files, since some may not exist in all
             ;; versions of python that are built with this recipe.
             (substitute* (filter file-exists?
                                  '("Lib/subprocess.py"
                                    "Lib/popen2.py"
                                    "Lib/distutils/tests/test_spawn.py"
                                    "Lib/test/support/__init__.py"
                                    "Lib/test/test_subprocess.py"))
               (("/bin/sh") (which "sh")))))
         (add-before 'configure 'do-not-record-configure-flags
           (lambda* (#:key configure-flags #:allow-other-keys)
             ;; Remove configure flags from the installed '_sysconfigdata.py'
             ;; and 'Makefile' so we don't end up keeping references to the
             ;; build tools.
             ;;
             ;; Preserve at least '--with-system-ffi' since otherwise the
             ;; thing tries to build libffi, fails, and we end up with a
             ;; Python that lacks ctypes.
             (substitute* "configure"
               (("^CONFIG_ARGS=.*$")
                (format #f "CONFIG_ARGS='~a'\n"
                        (if (member "--with-system-ffi" configure-flags)
                            "--with-system-ffi"
                            ""))))))
         (add-before 'check 'pre-check
           (lambda _
             ;; 'Lib/test/test_site.py' needs a valid $HOME
             (setenv "HOME" (getcwd))))
         (add-after 'unpack 'set-source-file-times-to-1980
           ;; XXX One of the tests uses a ZIP library to pack up some of the
           ;; source tree, and fails with "ZIP does not support timestamps
           ;; before 1980".  Work around this by setting the file times in the
           ;; source tree to sometime in early 1980.
           (lambda _
             (let ((circa-1980 (* 10 366 24 60 60)))
               (ftw "." (lambda (file stat flag)
                          (utime file circa-1980 circa-1980)
                          #t)))))
         (add-after 'install 'remove-tests
           ;; Remove 25 MiB of unneeded unit tests.  Keep test_support.*
           ;; because these files are used by some libraries out there.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (match (scandir (string-append out "/lib")
                               (lambda (name)
                                 (string-prefix? "python" name)))
                 ((pythonX.Y)
                  (let ((testdir (string-append out "/lib/" pythonX.Y
                                                "/test")))
                    (with-directory-excursion testdir
                      (for-each delete-file-recursively
                                (scandir testdir
                                         (match-lambda
                                           ((or "." "..") #f)
                                           ("support" #f)
                                           (file
                                            (not
                                             (string-prefix? "test_support."
                                                             file))))))
                      (call-with-output-file "__init__.py" (const #t)))))))))
         (add-before 'check 'set-TZDIR
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             ;; test_email requires the Olson time zone database.
             (setenv "TZDIR"
                     (string-append (assoc-ref
                                     (or native-inputs inputs) "tzdata")
                                    "/share/zoneinfo"))))
         ;; Unset SOURCE_DATE_EPOCH while running the test-suite and set it
         ;; again afterwards.  See <https://bugs.python.org/issue34022>.
         (add-before 'check 'unset-SOURCE_DATE_EPOCH
           (lambda _ (unsetenv "SOURCE_DATE_EPOCH")))
         (add-after 'check 'reset-SOURCE_DATE_EPOCH
           (lambda _ (setenv "SOURCE_DATE_EPOCH" "1")))
         (add-after 'remove-tests 'rebuild-bytecode
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Disable hash randomization to ensure the generated .pycs
               ;; are reproducible.
               (setenv "PYTHONHASHSEED" "0")
               (for-each
                (lambda (opt)
                  (format #t "Compiling with optimization level: ~a\n"
                          (if (null? opt) "none" (car opt)))
                  (for-each (lambda (file)
                              (apply invoke
                                     `(,,(if (%current-target-system)
                                             "python3"
                                             '(string-append out
                                                             "/bin/python3"))
                                       ,@opt
                                       "-m" "compileall"
                                       "-f" ; force rebuild
                                       ;; Don't build lib2to3, because it's Python 2 code.
                                       "-x" "lib2to3/.*"
                                       ,file)))
                            (find-files out "\\.py$")))
                (list '() '("-O") '("-OO"))))))
         (add-after 'install 'move-tk-inter
           (lambda* (#:key outputs #:allow-other-keys)
             ;; When Tkinter support is built move it to a separate output so
             ;; that the main output doesn't contain a reference to Tcl/Tk.
             (let ((out (assoc-ref outputs "out"))
                   (tk  (assoc-ref outputs "tk")))
               (when tk
                 (match (find-files out "tkinter.*\\.so")
                   ((tkinter.so)
                    ;; The .so is in OUT/lib/pythonX.Y/lib-dynload, but we
                    ;; want it under TK/lib/pythonX.Y/site-packages.
                    (let* ((len    (string-length out))
                           (target (string-append
                                    tk "/"
                                    (string-drop
                                     (dirname (dirname tkinter.so))
                                     len)
                                    "/site-packages")))
                      (install-file tkinter.so target)
                      (delete-file tkinter.so))))))))
         (add-after 'install 'install-sitecustomize.py
           ,(customize-site version)))))
    (inputs
     (list bzip2
           expat
           gdbm
           libffi                       ; for ctypes
           sqlite                       ; for sqlite extension
           openssl
           readline
           zlib
           tcl
           tk))
    (native-inputs
     `(("tzdata" ,tzdata-for-tests)
       ("pkg-config" ,pkg-config)
       ("sitecustomize.py" ,(local-file (search-auxiliary-file
                                         "python/sitecustomize.py")))
       ;; When cross-compiling, a native version of Python itself is needed.
       ,@(if (%current-target-system)
             `(("python3" ,this-package)
               ("which" ,which))
             '())))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_PYTHONPATH")
            (files (list (string-append "lib/python"
                                        (version-major+minor version)
                                        "/site-packages"))))))
    (home-page "https://www.python.org")
    (synopsis "High-level, dynamically-typed programming language")
    (description
     "Python is a remarkably powerful dynamic programming language that
is used in a wide variety of application domains.  Some of its key
distinguishing features include: clear, readable syntax; strong
introspection capabilities; intuitive object orientation; natural
expression of procedural code; full modularity, supporting hierarchical
packages; exception-based error handling; and very high level dynamic
data types.")
    (properties '((cpe-name . "python")))
    (license license:psfl)))



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
             (setenv "PYTHONPATH" (string-append (site-packages inputs outputs) ":"
                                                 (getenv "PYTHONPATH")))
             (when tests?
               ;; Taken from tox.ini
               (invoke "python" "test/test_argparse.py"))))
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))))
    (native-inputs
     `(("setuptools" ,python24-setuptools)))
    (home-page "https://github.com/ThomasWaldmann/argparse/")
    (properties '((release-date . "2015-09-12")))
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
             (setenv "PYTHONPATH" (string-append (site-packages inputs outputs) ":"
                                                 (getenv "PYTHONPATH")))
             (when tests?
               ;; Taken from tox.ini
               (invoke "python" "test.py"))))
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))))
    (native-inputs
     `(("setuptools" ,python24-setuptools)))
    (propagated-inputs
     `(("six" ,python24-six)))
    (home-page "https://dateutil.readthedocs.io/en/stable/")
    (properties '((release-date . "2012-03-28")))
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
               (("man/man1") "share/man/man1"))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             ;; This test fails, funtional_tests/support/empty isn't created.
             (delete-file "functional_tests/test_success.py")
             (when tests?
               (invoke "python" "selftest.py"))))
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))))
    (home-page "http://readthedocs.org/docs/nose/")
    (properties '((release-date . "2008-10-03")))
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
                                 lapack "/include', '" blas "/include']\n"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "python" "setup.py" "config"
                       "install" "--use_lapack"
                       (string-append "--prefix=" out)))))
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))
       #:tests? #f))   ; no test target
    (inputs
     `(("lapack" ,lapack)
       ("openblas" ,openblas)))
    (properties '((release-date . "2006-08-26")))
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
lapack openblas lapack openblas))))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (begin
                 ;; Taken from test.sh
                 (with-directory-excursion "/tmp"
                   (setenv "PYTHONPATH" (string-append (site-packages inputs outputs) ":"
                                                       (getenv "PYTHONPATH")))
                   (invoke "python" "-c"
                           "import numpy; print numpy; numpy.test(level = 9999); numpy.show_config()"))))))
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))))
    (properties '((release-date . "2007-11-08")))
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
    (properties '((release-date . "2008-07-31")))
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
    (properties '((release-date . "2008-10-28")))
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
               (invoke "python" "test.py"))))
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))))
    (native-inputs
     ;; Note: since Dec. 2021, 'texlive-union' is deprecated in favor of
     ;; 'texlive-updmap.cfg'.  Keep using it for now so that Guix-Past can be
     ;; used together with older Guix revisions.
     `(("texlive" ,(texlive-union (list texlive-bin
                                        texlive-cm
                                        texlive-latex-base)))))
    (properties '((release-date . "2012-10-26")))
    (home-page "https://pyx-project.org/")
    (synopsis "Create PostScript, PDF, and SVG files")
    (description "Pyx is a Python package for the generation of PostScript, PDF,
and SVG files.  It combines an abstraction of the PostScript drawing model with
a TeX/LaTeX interface.  Complex tasks like 2d and 3d plots in publication-ready
quality are built out of these primitives.")
    (license license:gpl2+)))

(define-public python24-pyxlwriter
  (package
    (name "python24-pyxlwriter")
    (version "0.4a3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/pyxlwriter/pyxlwriter/"
                            version "/pyXLWriter-" version ".zip"))
        (sha256
         (base32
          "1kfsi6la9y53rwayszgayfmkjfknpp650v69a0hwd1fcfk1df735"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))
       #:tests? #f))    ; Tests not included in tarball.
    (native-inputs
     `(("unzip" ,unzip)))
    (properties '((release-date . "2004-08-20")))
    (home-page "https://sourceforge.net/projects/pyxlwriter/")
    (synopsis "Python library for generating Excel compatible spreadsheets")
    (description "PyXLWriter is a Python library for generating Excel compatible
spreadsheets.  It's a port of John McNamara's Perl @code{Spreadsheet::WriteExcel}
module version 1.01 to Python.  It allows writing of Excel compatible
spreadsheets without the need for COM objects.")
    (license license:lgpl2.1+)))

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
       #:phases
       (modify-phases %standard-phases
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))
       #:tests? #f))    ; Tests want SVN and internet access.
    (properties '((release-date . "2013-12-01")))
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
           (delete-file-recursively "lib/pytz")))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))))
    (native-inputs
     `(("gcc" ,gcc-7)
       ("pkg-config" ,pkg-config)
       ("setuptools" ,python24-setuptools)))
    (propagated-inputs
     `(("dateutil" ,python24-dateutil)
       ("numpy" ,python24-numpy-1.1)
       ("pytz" ,python24-pytz)))
    (inputs
     `(("freetype" ,freetype)
       ("libpng" ,libpng)))
    (properties '((release-date . "2011-11-14")))
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
       #:phases
       (modify-phases %standard-phases
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))
       #:tests? #f))    ; Tests rely on pytest
    (native-inputs
     `(("setuptools" ,python24-setuptools)))
    (home-page "https://py.readthedocs.io/en/latest/")
    (properties '((release-date . "2015-11-27")))
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
             (setenv "PYTHONPATH" (string-append (site-packages inputs outputs) ":"
                                                 (getenv "PYTHONPATH")))
               (when tests?
                 ;; Taken from tox.ini
                 (with-directory-excursion "testing"
                   (invoke "python" "-m" "pytest" "--lsof" "-rfsxX")))))
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))))
    (propagated-inputs
     `(("argparse" ,python24-argparse)  ; python < 2.7
       ("py" ,python24-py)))
    (native-inputs
     `(("setuptools" ,python24-setuptools)))
    (home-page "https://docs.pytest.org/en/stable/")
    (properties '((release-date . "2013-10-04")))
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
     `(#:python ,python-2.4
       #:phases
       (modify-phases %standard-phases
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))))
    (native-inputs
     `(("setuptools" ,python24-setuptools)))))

(define-public python24-rpy2
  (package
    (inherit python-rpy2)
    (name "python24-rpy2")
    (version "2.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rpy2" version))
        (sha256
         (base32
          "0g6vmv4pxc9bwb756z1vfdlzviib84afjmp4l5cw22x8qqvw1s9s"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2.4
       #:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "PYTHONPATH" (string-append (site-packages inputs outputs) ":"
                                                 (getenv "PYTHONPATH")))
             (with-directory-excursion "rpy"
               (invoke "python" "tests_rpy_classic.py"))))
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))))
    (inputs `())
    (propagated-inputs
     `(("python-numpy" ,python24-numpy-1.2)
       ("r-minimal" ,r-minimal-2)
       ("r-survival" ,r-2-survival)))
    (properties '((release-date . "2009-11-06")))))

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
             (setenv "PYTHONPATH" (string-append (site-packages inputs outputs) ":"
                                                 (getenv "PYTHONPATH")))
             (when tests?
               ;; Taken from tox.ini
               (invoke "py.test" "-rfsxX"))))
         (delete 'add-install-to-pythonpath)
         (delete 'sanity-check))))
    (home-page "https://pypi.org/project/six/")
    (native-inputs
     `(("pytest" ,python24-pytest)
       ("setuptools" ,python24-setuptools)))
    (properties '((release-date . "2013-09-02")))
    (synopsis "Six 1.4.1, released 2013-09-02")
    (description
     "Six is a Python 2 and 3 compatibility library.  It provides utility
functions for smoothing over the differences between the Python versions with
the goal of writing Python code that is compatible on both Python versions.")
    (license license:expat)))
