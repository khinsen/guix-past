;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (past packages python27)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python))

(define %python2-package-mapping
  ;; Hash table that maps a Python 3.x package to its Python 2.x counterpart.
  (make-hash-table))

(define-syntax define-python2-package
  (lambda (x)
    (syntax-case x ()
      ((_ variable value)
       (let ((name (symbol->string (syntax->datum #'variable))))
         #`(begin
             (hash-set! %python2-package-mapping
                        (string-append "python-"
                                       (string-drop #,name
                                                    (string-length "python2-")))
                        (delay value))
             (define-public variable value)))))))

(define (python2-package-variant package)
  "Return the promise of a Python 2.x variant of PACKAGE."
  (or (hash-ref %python2-package-mapping (package-name package))
      (assq-ref (package-properties package) 'python2-variant)))

(define* (package-with-explicit-python python old-prefix new-prefix)
  "Return a procedure of one argument, P.  The procedure creates a package with
the same fields as P, which is assumed to use PYTHON-BUILD-SYSTEM, such that
it is compiled with PYTHON instead.  The inputs are changed recursively
accordingly.  If the name of P starts with OLD-PREFIX, this is replaced by
NEW-PREFIX; otherwise, NEW-PREFIX is prepended to the name."
  (define (transform p)
    (cond
     ;; Use a package variant if it exists
     ((python2-package-variant p)
      => force)

     ;; Otherwise build the new package object graph.
     ((eq? (package-build-system p) python-build-system)
      (package/inherit p
        (location (package-location p))
        (name (let ((name (package-name p)))
                (if (string-prefix? old-prefix name)
                    (string-append new-prefix
                                   (substring name (string-length old-prefix)))
                    name)))
        (arguments
         (let ((python (if (promise? python)
                           (force python)
                           python)))
           (ensure-keyword-arguments (package-arguments p)
                                     `(#:python ,python))))))
     (else p)))

  (define (cut? p)
    (or (not (eq? (package-build-system p) python-build-system))
        (python2-package-variant p)))

  (package-mapping transform cut?))

(define python2-package
  ;; Note: delay call to 'default-python2' until after the 'arguments' field
  ;; of packages is accessed to avoid a circular dependency when evaluating
  ;; the top-level of (gnu packages python).
  (package-with-explicit-python (delay (default-python2))
                                "python-" "python2-"))

(define S specification->package)
(define (S2 spec)
  (cond
   ((hash-ref %python2-package-mapping spec) => force)
   (else
    ((compose python2-package specification->package) spec))))


(define-python2-package python2-six
  (package
    (name "python2-six")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "six" version))
       (sha256
        (base32
         "09n9qih9rpj95q3r4a40li7hk6swma11syvgwdc68qm1fxsc6q8y"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2 #:tests? #f)) ;to avoid pytest dependency
    (home-page "https://pypi.org/project/six/")
    (synopsis "Python 2 and 3 compatibility utilities")
    (description
     "Six is a Python 2 and 3 compatibility library.  It provides utility
functions for smoothing over the differences between the Python versions with
the goal of writing Python code that is compatible on both Python versions.
Six supports every Python version since 2.5.  It is contained in only one
Python file, so it can be easily copied into your project.")
    (license license:x11)))

(define-python2-package python2-funcsigs
  (package
    (name "python2-funcsigs")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "funcsigs" version))
              (sha256
               (base32
                "0l4g5818ffyfmfs1a924811azhjj8ax9xd1cffr1mzd3ycn0zfx7"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #false            ;keep it simple to avoid a dependency cycle
       #:python ,python-2))
    (home-page "http://funcsigs.readthedocs.org")
    (synopsis "Python function signatures from PEP362")
    (description
     "Backport of @code{funcsigs} which was introduced in Python 3.3.")
    (license license:asl2.0)))

;; The 5.x series are the last versions supporting Python 2.7.
(define-python2-package python2-more-itertools
  (package
    (name "python2-more-itertools")
    (version "5.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "more-itertools" version))
              (sha256
               (base32
                "1r12cm6mcdwdzz7d47a6g4l437xsvapdlgyhqay3i2nrlv03da9q"))))
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     (list (S2 "python-six")))
    (build-system python-build-system)
    (home-page "https://github.com/erikrose/more-itertools")
    (synopsis "More routines for operating on iterables, beyond itertools")
    (description "Python's built-in @code{itertools} module implements a
number of iterator building blocks inspired by constructs from APL, Haskell,
and SML.  @code{more-itertools} includes additional building blocks for
working with iterables.")
    (license license:expat)))

;;; This is the last release compatible with Python 2.
(define-python2-package python2-pyparsing
  (package
    (name "python2-pyparsing")
    (version "2.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyparsing" version))
       (sha256
        (base32 "1hgc8qrbq1ymxbwfbjghv01fm3fbpjwpjwi0bcailxxzhf3yq0y2"))))
    (build-system python-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:tests? #f                      ;no test target
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((doc (string-append (assoc-ref outputs "doc")
                                        "/share/doc/" ,name "-" ,version))
                    (html-doc (string-append doc "/html"))
                    (examples (string-append doc "/examples")))
               (mkdir-p html-doc)
               (mkdir-p examples)
               (for-each
                (lambda (dir tgt)
                  (map (lambda (file)
                         (install-file file tgt))
                       (find-files dir ".*")))
                (list "docs" "htmldoc" "examples")
                (list doc html-doc examples))))))))
    (home-page "https://github.com/pyparsing/pyparsing")
    (synopsis "Python parsing class library")
    (description
     "The pyparsing module is an alternative approach to creating and
executing simple grammars, vs. the traditional lex/yacc approach, or the use
of regular expressions.  The pyparsing module provides a library of classes
that client code uses to construct the grammar directly in Python code.")
    (license license:expat)))

(define-python2-package python2-packaging-bootstrap
  (package
    (name "python2-packaging-bootstrap")
    (version "20.0")               ;last version with Python 2 support
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "packaging" version))
       ;; XXX: The URL in the patch file is wrong, it should be
       ;; <https://github.com/pypa/packaging/pull/256>.
       (patches (search-patches "python-packaging-test-arch.patch"))
       (sha256
        (base32
         "1y2ip3a4ykkpgnwgn85j6hkspcl0cg3mzms97f40mk57vwqq67gy"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f))   ;disabled to avoid extra dependencies
    (propagated-inputs
     (map S2 (list "python-pyparsing" "python-six")))
    (home-page "https://github.com/pypa/packaging")
    (synopsis "Core utilities for Python packages")
    (description "Packaging is a Python module for dealing with Python packages.
It offers an interface for working with package versions, names, and dependency
information.")
    (license (list license:asl2.0 license:bsd-2))))

;; This minimal variant is used to avoid a circular dependency between
;; python2-importlib-metadata, which requires pyfakefs for its tests, and
;; python2-pytest, which requires python2-importlib-metadata.
(define-python2-package python2-pyfakefs-bootstrap
  (package
    (name "python2-pyfakefs-bootstrap")
    (version "3.7.1")
    (source (origin
              (method url-fetch)
              ;; We use the PyPI URL because there is no proper release
              ;; available from GitHub.  The GitHub project only provides
              ;; autogenerated tarballs, which are known to change in place.
              (uri (pypi-uri "pyfakefs" version))
              (sha256
               (base32
                "1cp2yw96fa2qkgi39xa3nlr3inf8wb5rgh9kdq53256ca2r8pdhy"))
              (patches (search-patches
                        "python-pyfakefs-remove-bad-test.patch"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f))
    ;; Guix lint doesn't like that this is a permanent redirect to the GitHub
    ;; page, but the pyfakefs documentation asks us to use this specific URL
    ;; when linking to the project.  Honor their request.
    (home-page "http://pyfakefs.org/")
    ;; TRANSLATORS: In the synopsis, "Mock" is a verb.
    (synopsis "Mock file system interactions in tests")
    (description
     "This package provides a Python library intended for use in automated
tests.  One difficulty when testing software is that the code under test might
need to read or write to files in the local file system.  If the file system
is not set up in just the right way, it might cause a spurious error during
the test.  The pyfakefs library provides a solution to problems like this by
mocking file system interactions.  In other words, it arranges for the code
under test to interact with a fake file system instead of the real file
system.  The code under test requires no modification to work with pyfakefs.")
    (license license:asl2.0)))

(define-python2-package python2-pathlib2
  (package
    (name "python2-pathlib2")
    (version "2.3.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pathlib2" version))
              (sha256
               (base32
                "0s4qa8c082fdkb17izh4mfgwrjd1n5pya18wvrbwqdvvb5xs9nbc"))))
    (build-system python-build-system)
    ;; We only need the the Python 2 variant, since for Python 3 our minimum
    ;; version is 3.4 which already includes this package as part of the
    ;; standard library.
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     (map S2 (list "python-scandir" "python-six")))
    (home-page "https://pypi.org/project/pathlib2/")
    (synopsis "Object-oriented file system paths - backport of standard
pathlib module")
    (description "The goal of pathlib2 is to provide a backport of standard
pathlib module which tracks the standard library module, so all the newest
features of the standard pathlib can be used also on older Python versions.

Pathlib offers a set of classes to handle file system paths.  It offers the
following advantages over using string objects:

@enumerate
@item No more cumbersome use of os and os.path functions.  Everything can
be done easily through operators, attribute accesses, and method calls.
@item Embodies the semantics of different path types.  For example,
comparing Windows paths ignores casing.
@item Well-defined semantics, eliminating any inconsistencies or
ambiguities (forward vs. backward slashes, etc.).
@end enumerate")
    (license license:expat)))

(define-python2-package python2-pathlib2-bootstrap
  (package
    (inherit python2-pathlib2)
    (name "python2-pathlib2-bootstrap")
    (propagated-inputs
     (map S2 (list "python-scandir"
                   "python-six-bootstrap")))))

(define-python2-package python2-setuptools-scm
  (package
    (name "python2-setuptools-scm")
    (version "5.0.2")                ;no python 2 support in version 6
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "setuptools_scm" version))
              (sha256
               (base32
                "1j75i8avp9fhrkpbabsa8vyvbi49kmxlq6l10xir9qs96kfwx843"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (home-page "https://github.com/pypa/setuptools_scm/")
    (synopsis "Manage Python package versions in SCM metadata")
    (description
     "Setuptools_scm handles managing your Python package versions in
@dfn{software configuration management} (SCM) metadata instead of declaring
them as the version argument or in a SCM managed file.")
    (license license:expat)))

(define-python2-package python2-contextlib2
  (package
    (name "python2-contextlib2")
    (version "0.6.0.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "contextlib2" version))
       (sha256
        (base32
         "0bhnr2ac7wy5l85ji909gyljyk85n92w8pdvslmrvc8qih4r1x01"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "test_contextlib2.py" "-v")))))))
    (native-inputs
     (list (S2 "python-unittest2")))
    (home-page "https://contextlib2.readthedocs.org/")
    (synopsis "Tools for decorators and context managers")
    (description "This module is primarily a backport of the Python
3.2 contextlib to earlier Python versions.  Like contextlib, it
provides utilities for common tasks involving decorators and context
managers.  It also contains additional features that are not part of
the standard library.")
    (license license:psfl)))

;; This package is used by python2-pytest via python2-importlib-metadata,
;; and thus can not depend on python-unittest2 (which depends on pytest).
(define-python2-package python2-contextlib2-bootstrap
  (package/inherit python2-contextlib2
    (name "python2-contextlib2-bootstrap")
    (arguments
     `(#:tests? #f
       ,@(package-arguments python2-contextlib2)))
    (native-inputs '())))

(define-python2-package python2-typing
  (package
    (name "python2-typing")
    (version "3.10.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "typing" version))
       (sha256
        (base32 "0c5il4d68fd4qrm5k3dps70j0xz0n5krj6lhwn9vzpal3whsvd0k"))))
    (build-system python-build-system)
    (arguments (list #:python python-2))
    (home-page "https://docs.python.org/3/library/typing.html")
    (synopsis "Type hints for Python")
    (description "This is a backport of the standard library @code{typing}
module to Python versions older than 3.5.  Typing defines a standard notation
for Python function and variable type annotations.  The notation can be used
for documenting code in a concise, standard format, and it has been designed
to also be used by static and runtime type checkers, static analyzers, IDEs
and other tools.")
    (license license:psfl)))

(define-python2-package python2-importlib-resources
  (package
    (name "python2-importlib-resources")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "importlib_resources" version))
              (sha256
               (base32
                "0y3hg12iby1qyaspnbisz4s4vxax7syikk3skznwqizqyv89y9yk"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases (modify-phases %standard-phases
                  ;; The build system tests for python-wheel, but it is
                  ;; not required for Guix nor the test suite.  Just drop
                  ;; it to make bootstrapping pytest easier.
                  (add-after 'unpack 'drop-wheel-dependency
                    (lambda _
                      (substitute* "setup.cfg"
                        (("^[[:blank:]]+wheel")
                         "")))))))
    (propagated-inputs
     (map S2 (list "python-typing"
                   "python-pathlib2")))
    (home-page "https://gitlab.com/python-devs/importlib_resources")
    (synopsis "Backport of @code{importlib.resources} from Python 3.7")
    (description
     "This package provides an implementation of @code{importlib.resources}
for older versions of Python.")
    (license license:asl2.0)))
 
;; For importlib-metadata-bootstrap below.
(define-python2-package python2-importlib-resources-bootstrap
  (hidden-package
   (package/inherit
    python2-importlib-resources
    (name "python2-importlib-resources-bootstrap")
    (propagated-inputs
     (map S2 (list "python-pathlib2-bootstrap" "python-typing"))))))

(define-python2-package python2-zipp
  (package
    (name "python2-zipp")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "zipp" version))
       (sha256
        (base32
         "0v3qayhqv7vyzydpydwcp51bqciw8p2ajddw68x5k8zppc0vx3yk"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (propagated-inputs
     (list (S2 "python-more-itertools")))
    (native-inputs
     (map S2 (list "python-contextlib2"
                   "python-pathlib2"
                   "python-unittest2"
                   "python-setuptools-scm")))
    (home-page "https://github.com/jaraco/zipp")
    (synopsis
     "Backport of pathlib-compatible object wrapper for zip files")
    (description
     "This package provides a @code{pathlib}-compatible @code{Zipfile} object
wrapper.  It provides a backport of the @code{Path} object.")
    (license license:expat)))

;; This package is used to bootstrap pytest, via importlib-metadata.
(define-python2-package python2-zipp-bootstrap
  (hidden-package
   (package/inherit
    python2-zipp
    (name "python2-zipp-bootstrap")
    (arguments
     `(#:tests? #f
       ,@(package-arguments python2-zipp)))
    (native-inputs
     (list (S2 "python-setuptools-scm"))))))

(define-python2-package python2-importlib-metadata
  (package
    (name "python2-importlib-metadata")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "importlib_metadata" version))
       (sha256
        (base32
         "00ikdj4gjhankdljnz7g5ggak4k9lql2926x0x117ir9j2lv7x86"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (native-inputs
     (map S2 (list "python-setuptools-scm"
                   "python-pyfakefs-bootstrap"
                   "python-packaging-bootstrap")))
    (propagated-inputs
     (map S2 (list "python-zipp"
                   "python-configparser"
                   "python-contextlib2"
                   "python-importlib-resources"
                   "python-pathlib2")))
    (home-page "https://importlib-metadata.readthedocs.io/")
    (synopsis "Read metadata from Python packages")
    (description
     "@code{importlib_metadata} is a library which provides an API for
accessing an installed Python package's metadata, such as its entry points or
its top-level name.  This functionality intends to replace most uses of
@code{pkg_resources} entry point API and metadata API.  Along with
@code{importlib.resources} in Python 3.7 and newer, this can eliminate the
need to use the older and less efficient @code{pkg_resources} package.")
    (license license:asl2.0)))

;; This package is used by python2-pytest, and thus must not depend on it.
(define-python2-package python2-importlib-metadata-bootstrap
  (package/inherit python2-importlib-metadata
    (name "python2-importlib-metadata-bootstrap")
    (arguments
     `(#:tests? #f
       ,@(package-arguments python2-importlib-metadata)))
    (propagated-inputs
     (map S2 (list "python-zipp-bootstrap"
                   "python-pathlib2-bootstrap"
                   "python-contextlib2-bootstrap"
                   "python-importlib-resources-bootstrap"
                   "python-configparser")))))


(define-python2-package python2-pluggy
  (package
   (name "python2-pluggy")
   (version "0.13.1")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "pluggy" version))
     (sha256
      (base32
       "1c35qyhvy27q9ih9n899f3h4sdnpgq027dbiilly2qb5cvgarchm"))))
   (build-system python-build-system)
   (arguments `(#:python ,python-2))
   (native-inputs
    (list python2-setuptools-scm))
   (propagated-inputs
     (list python2-importlib-metadata))
   (synopsis "Plugin and hook calling mechanism for Python")
   (description "Pluggy is an extraction of the plugin manager as used by
Pytest but stripped of Pytest specific details.")
   (home-page "https://pypi.org/project/pluggy/")
   (license license:expat)))

;; This package requires python2-importlib-metadata, but that package
;; ends up needing python2-pluggy via python2-pytest, so we need this
;; variant to solve the circular dependency.
(define-python2-package python2-pluggy-bootstrap
  (package/inherit python2-pluggy
    (name "python2-pluggy-bootstrap")
    (arguments
     `(#:tests? #f
       ,@(package-arguments python2-pluggy)))
    (propagated-inputs
     (list python2-importlib-metadata-bootstrap))))

;; Pytest 4.x are the last versions that support Python 2.
(define-python2-package python2-pytest
  (package
    (name "python2-pytest")
    (version "4.6.11")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest" version))
              (sha256
               (base32
                "0ls3pqr86xgif6bphsb6wrww9r2vc7p7a2naq8zcq8115wwq5yjh"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #false
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'pretend-version
           ;; The version string is usually derived via setuptools-scm, but
           ;; without the git metadata available, the version string is set to
           ;; '0.0.0'.
           (lambda _
             (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" ,version)))
         (replace 'check
           (lambda* (#:key (tests? #t) #:allow-other-keys)
             (setenv "TERM" "dumb")    ;attempt disabling markup tests
             (if tests?
                 (invoke "pytest" "-vv" "-k"
                         (string-append
                          ;; This test involves the /usr directory, and fails.
                          " not test_argcomplete"
                          ;; These test do not honor the isatty detection and
                          ;; fail.
                          " and not test_code_highlight"
                          " and not test_color_yes"))
                 (format #t "test suite not run~%")))))))
    (propagated-inputs
     (map S2
          (list "python-atomicwrites"
                "python-attrs-bootstrap"
                "python-funcsigs"
                "python-importlib-metadata-bootstrap"
                "python-more-itertools"
                "python-packaging-bootstrap"
                "python-pathlib2"
                "python-pluggy"
                "python-py"
                "python-six-bootstrap"
                "python-wcwidth")))
    (native-inputs
     (cons (specification->package "bash") ;tests require 'compgen'
           (map S2
                (list ;"python-hypothesis"
                      ;"python-nose"
                      ;"python-mock"
                      "python-setuptools-scm"))))
    (home-page "https://docs.pytest.org/en/latest/")
    (synopsis "Python testing library")
    (description
     "Pytest is a testing tool that provides auto-discovery of test modules
and functions, detailed info on failing assert statements, modular fixtures,
and many external plugins.")
    (license license:expat))) 

(define-python2-package python2-cython
  (package
    (name "python2-cython")
    (version "0.29.24")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Cython" version))
       (sha256
        (base32 "0hw4gs18rh4slij1fg252argxhraypld9apbqbl60230qc3lvw6d"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-HOME
           ;; some tests require access to "$HOME/.cython"
           (lambda _ (setenv "HOME" "/tmp")))
         (add-before 'check 'adjust-test_embed
           (lambda _
             (substitute* "runtests.py"
               ;; test_embed goes great lengths to find the static libpythonX.Y.a
               ;; so it can give the right -L flag to GCC when embedding static
               ;; builds of Python.  It is unaware that the Python "config"
               ;; directory (where the static library lives) was renamed in
               ;; Python 3, and falls back to sysconfig.get_config_var('LIBDIR'),
               ;; which works fine, because that is where the shared library is.
               ;;
               ;; It also appears to be unaware that the Makefile in Demos/embed
               ;; already unconditionally pass the static library location to GCC,
               ;; after checking sysconfig.get_config_var('LIBPL).
               ;;
               ;; The effect is that the linker is unable to resolve libexpat
               ;; symbols when building for Python 2, because neither the Python 2
               ;; shared library nor Expat is available.   To fix it, we can either
               ;; add Expat as an input and make it visible to the linker, or just
               ;; prevent it from overriding the Python shared library location.
               ;; The end result is identical, so we take the easy route.
               ((" or libname not in os\\.listdir\\(libdir\\)")
                ""))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             ;; Disable compiler optimizations to greatly reduce the running
             ;; time of the test suite.
             (setenv "CFLAGS" "-O0")

             (when tests?
               (invoke "python" "runtests.py" "-vv"
                       "-j" (number->string (parallel-job-count))
                       ;; XXX: On 32-bit architectures, running the parallel tests
                       ;; fails on many-core systems, see
                       ;; <https://github.com/cython/cython/issues/2807>.
                       ,@(if (not (target-64bit?))
                             '("-x" "run.parallel")
                             '())
                       ;; This test fails when running on 24 cores.
                       "-x" "cpp_stl_conversion")))))))
    ;; we need the full python package and not just the python-wrapper
    ;; because we need libpython2.7m.so
    (inputs (list python-2))
    (home-page "https://cython.org/")
    (synopsis "C extensions for Python")
    (description "Cython is an optimising static compiler for both the Python
programming language and the extended Cython programming language.  It makes
writing C extensions for Python as easy as Python itself.")
    (license license:asl2.0)))

;; Numpy 1.16.x is the last version that supports Python 2.
(define-python2-package python2-numpy
  (package
    (name "python2-numpy")
    (version "1.16.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/numpy/numpy/releases/download/v"
                    version "/numpy-" version ".tar.gz"))
              (sha256
               (base32
                "0lg1cycxzi4rvvrd5zxinpdz0ni792fpx6xjd75z1923zcac8qrb"))))
    (build-system python-build-system)
    (arguments
     (list
      #:python python-2
      #:modules '((guix build utils)
                  (guix build python-build-system)
                  (ice-9 format))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'parallelize-build
            (lambda _
              (setenv "NPY_NUM_BUILD_JOBS"
                      (number->string (parallel-job-count)))))
          (add-before 'build 'configure-blas
            (lambda* (#:key inputs #:allow-other-keys)
              (call-with-output-file "site.cfg"
                (lambda (port)
                  (format port
                          "\
[openblas]
libraries = openblas
library_dirs = ~a/lib
include_dirs = ~:*~a/include~%" #$(this-package-input "openblas"))))))
          (add-before 'build 'fix-executable-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Make /gnu/store/...-bash-.../bin/sh the default shell,
              ;; instead of /bin/sh.
              (substitute* "numpy/distutils/exec_command.py"
                (("'/bin/sh'")
                 (format #f "~s" (search-input-file inputs "bin/bash"))))))
          (add-after 'unpack 'delete-failing-tests
            (lambda _
              ;; There's just one failing test here.
              (delete-file "numpy/linalg/tests/test_linalg.py")
              ;; ...and this one depends on the previous one.
              (delete-file "numpy/matrixlib/tests/test_matrix_linalg.py")))
          (replace 'check
            ;; Older versions don't cope well with the extra Pytest
            ;; options, so remove them.
            (lambda* (#:key tests? outputs inputs #:allow-other-keys)
              (when tests?
                (invoke "./runtests.py" "-vv" "--no-build" "--mode=fast"
                        "-j" (number->string (parallel-job-count)))))))))
    (inputs
     (map specification->package
          (list "bash" "openblas")))
    (native-inputs
     (cons gfortran
           (map S2 (list "python-cython" "python-pytest"))))
    (home-page "https://numpy.org")
    (synopsis "Fundamental package for scientific computing with Python")
    (description "NumPy is the fundamental package for scientific computing
with Python.  It contains among other things: a powerful N-dimensional array
object, sophisticated (broadcasting) functions, tools for integrating C/C++
and Fortran code, useful linear algebra, Fourier transform, and random number
capabilities.")
    (license license:bsd-3)))

(define-python2-package python2-pillow
  (package
    (name "python2-pillow")
    ;; Version 6 is the last series with Python 2 support.
    (version "6.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Pillow" version))
       (sha256
        (base32
         "0l5rv8jkdrb5q846v60v03mcq64yrhklidjkgwv6s1pda71g17yv"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #false            ;FIXME: One of the tests is failing.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-ldconfig
           (lambda _
             (substitute* "setup.py"
               (("\\['/sbin/ldconfig', '-p'\\]") "['true']"))))
         (replace 'check
           (lambda* (#:key outputs inputs tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME"
                       (getcwd))
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "selftest.py" "--installed")
               (invoke "python" "-m" "pytest" "-vv")))))))
    (native-inputs
     (list (S "python-pytest")))
    (inputs
     (list (S "freetype")
           (S "lcms")
           (S "libjpeg-turbo")
           (S "libtiff")
           (S "libwebp")
           (S "openjpeg")
           (S "zlib")))
    (propagated-inputs
     (list (S2 "python-olefile")))
    (home-page "https://python-pillow.org")
    (synopsis "Fork of the Python Imaging Library")
    (description
     "The Python Imaging Library adds image processing capabilities to your
Python interpreter.  This library provides extensive file format support, an
efficient internal representation, and fairly powerful image processing
capabilities.  The core image library is designed for fast access to data
stored in a few basic pixel formats.  It should provide a solid foundation for
a general image processing tool.")
    (properties `((cpe-name . "pillow")))
    (license (license:x11-style
              "http://www.pythonware.com/products/pil/license.htm"
              "The PIL Software License"))))
