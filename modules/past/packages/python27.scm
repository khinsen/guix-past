;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Konrad Hinsen <konrad.hinsen@fastmail.net>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python)
  #:use-module (gnu packages maths)
  #:export (python2-package))

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

;; Newer versions of setuptools no longer support Python 2.
(define-python2-package python2-setuptools
  (package
    (name "python2-setuptools")
    (version "41.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "setuptools" version ".zip"))
       (sha256
        (base32
         "04sns22y2hhsrwfy1mha2lgslvpjsjsz8xws7h2rh5a7ylkd28m2"))
       (modules '((guix build utils)))
       ;; Remove included binaries which are used to build self-extracting
       ;; installers for Windows.
       ;; TODO: Find some way to build them ourself so we can include them.
       (snippet
        '(for-each delete-file (find-files "setuptools" "^(cli|gui).*\\.exe$")))))
    (build-system python-build-system)
    ;; FIXME: Tests require pytest, which itself relies on setuptools.
    ;; One could bootstrap with an internal untested setuptools.
    (arguments
     `(#:tests? #f
       #:python ,python-2))
    (native-inputs (list (S "unzip")))
    (home-page "https://pypi.org/project/setuptools/")
    (synopsis "Library designed to facilitate packaging Python projects")
    (description "Setuptools is a fully-featured, stable library designed to
facilitate packaging Python projects, where packaging includes:
@itemize
@item Python package and module definitions
@item distribution package metadata
@item test hooks
@item project installation
@item platform-specific details.
@end itemize")
    ;; TODO: setuptools now bundles the following libraries:
    ;; packaging, pyparsing, six and appdirs. How to unbundle?
    (license (list license:psfl         ;setuptools itself
                   license:expat        ;six, appdirs, pyparsing
                   license:asl2.0       ;packaging is dual ASL2/BSD-2
                   license:bsd-2))))

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

(define-python2-package python2-enum34
  (package
    (name "python2-enum34")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "enum34" version))
       (sha256
        (base32
         "1cgm5ng2gcfrkrm3hc22brl6chdmv67b9zvva9sfs7gn7dwc9n4a"))))
    (build-system python-build-system)
    (arguments (list #:python python-2))
    (home-page "https://pypi.org/project/enum34/")
    (synopsis "Backported Python 3.4 Enum")
    (description
     "Enum34 is the new Python stdlib enum module available in Python 3.4
backported for previous versions of Python from 2.4 to 3.3.")
    (license license:bsd-3)))

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

(define-python2-package python2-pytest-cov
  (package
    (name "python2-pytest-cov")
    (version "2.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-cov" version))
       (sha256
        (base32 "0avzlk9p4nc44k7lpx9109dybq71xqnggxb9f4hp0l64pbc44ryc"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Options taken from tox.ini.
             ;; TODO: make "--restructuredtext" tests pass. They currently fail
             ;; with "Duplicate implicit target name".
             (invoke "python" "./setup.py" "check"
                     "--strict" "--metadata"))))))
    (propagated-inputs
     (map S2 (list "python-coverage" "python-pytest")))
    (home-page "https://github.com/pytest-dev/pytest-cov")
    (synopsis "Pytest plugin for measuring coverage")
    (description
     "Pytest-cov produces coverage reports.  It supports centralised testing and
distributed testing in both @code{load} and @code{each} modes.  It also
supports coverage of subprocesses.")
    (license license:expat)))

(define-python2-package python2-pytest-mock
  (package
    (name "python2-pytest-mock")
    (version "1.13.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pytest-mock" version))
              (sha256
               (base32
                "1mdfwqswc3ipgjdx3xflaq5lilvwnmch00y7php04wv7r4g92jp2"))))
    (build-system python-build-system)
    (arguments (list #:python python-2))
    (propagated-inputs
     (map S2 (list "python-mock" "python-pytest")))
    (native-inputs
     (list (S2 "python-setuptools-scm")))
    (home-page "https://github.com/pytest-dev/pytest-mock/")
    (synopsis "Thin-wrapper around the mock package for easier use with pytest")
    (description
     "Thin-wrapper around the mock package for easier use with pytest")
    (license license:expat)))

(define-python2-package python2-mock
  (package
    (name "python2-mock")
    (version "3.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mock" version))
       (sha256
        (base32 "1hrp6j0yrx2xzylfv02qa8kph661m6yq4p0mc8fnimch9j4psrc3"))))
    (build-system python-build-system)
    (arguments
     (list #:python python-2
           ;; Tests require "pytest", which depends on this package.
           #:tests? #f))
    (propagated-inputs
     (map S2 (list "python-funcsigs" "python-six")))
    (home-page "https://github.com/testing-cabal/mock")
    (synopsis "Python mocking and patching library for testing")
    (description
     "Mock is a library for testing in Python.  It allows you to replace parts
of your system under test with mock objects and make assertions about how they
have been used.  This library is now part of Python (since Python 3.3),
available via the @code{unittest.mock} module.")
    (license license:expat)))

;; This is the last version of Hypothesis that supports Python 2.
(define-python2-package python2-hypothesis
  (package
    (name "python2-hypothesis")
    (version "4.57.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hypothesis" version))
              (sha256
               (base32
                "183gpxbfcdhdqzlahkji5a71n6lmvgqsbkcb0ihqad51n2j6jhrw"))))
    (build-system python-build-system)
    (arguments
     ;; XXX: Tests are not distributed with the PyPI archive.
     (list #:tests? #f
           #:python python-2))
    (propagated-inputs
     (map S2 (list "python-enum34"
                   "python-attrs-bootstrap"
                   "python-sortedcontainers")))
    (synopsis "Library for property based testing")
    (description "Hypothesis is a library for testing your Python code against a
much larger range of examples than you would ever want to write by hand.  It’s
based on the Haskell library, Quickcheck, and is designed to integrate
seamlessly into your existing Python unit testing work flow.")
    (home-page "https://github.com/HypothesisWorks/hypothesis")
    (license license:mpl2.0)))

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
     `(#:python ,python-2
       #:phases
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

;;; This package is unmaintained (see the note at the top of doc/index.rst).
(define-python2-package python2-nose
  (package
    (name "python2-nose")
    (version "1.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "nose" version))
        (sha256
          (base32
            "164a43k7k2wsqqk1s6vavcdamvss4mz0vd6pwzv2h9n8rgwzxgzi"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f
           #:python python-2))
    (home-page "http://readthedocs.org/docs/nose/")
    (synopsis "Python testing library")
    (description
     "Nose extends the unittest library to make testing easier.")
    (license license:lgpl2.0+)))

;; Numpy 1.16.x is the last version that supports Python 2.
(define-python2-package python2-numpy
  (package
    (name "python2-numpy")
    (version "1.16.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/numpy/numpy/releases/download/v"
                    version "/numpy-" version ".tar.gz"))
              (sha256
               (base32
                "0xw7y0yk1rgsga9c6bmixrvpvfprqslhs2shb6crcrc7qzd2smk1"))
              (snippet
               #~(begin
                   ;; Remove the pre-cythonized files.
                   (delete-file "./numpy/random/mtrand/mtrand.c")))))
    (build-system python-build-system)
    (arguments
     (list
      #:python python-2
      #:modules '((guix build utils)
                  (guix build python-build-system)
                  (ice-9 format))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'cythonize-files
            (lambda _
              (with-directory-excursion "numpy/random/mtrand"
                (invoke "python" "generate_mtrand_c.py"))))
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

;; Numpy 1.8.2 is the last version that supports the compatibility interface
;; with NumPy's predecessor Numeric.
(define-python2-package python2-numpy-1.8
  (package
    (name "python2-numpy")
    (version "1.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/numpy/numpy/archive/v" version ".tar.gz"))
       (file-name (string-append "python2-numpy-" version ".tar.gz"))
       (sha256
        (base32
         "0sc20gz1b17xnyrkp5frca3ql5qfalpv916hfg2kqxpwr6jg0f1g"))))
    (build-system python-build-system)
    (arguments
     (list
      #:python python-2
      #:modules '((guix build utils)
                  (guix build python-build-system)
                  (ice-9 format))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'cythonize-files
            (lambda _
              (with-directory-excursion "numpy/random/mtrand"
                (invoke "python" "generate_mtrand_c.py"))))
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
          ;; (add-after 'unpack 'delete-failing-tests
          ;;   (lambda _
          ;;     ;; test_who_with_0dim_array fails with the message
          ;;     ;; "ticket #1243", which suggests it was a known problem
          ;;     ;; at the time.
          ;;     (delete-file "numpy/lib/tests/test_regression.py")))
          (replace 'check
            ;; Older versions don't cope well with the extra Pytest
            ;; options, so remove them.
            (lambda* (#:key tests? outputs inputs #:allow-other-keys)
              (when tests?
                (invoke "./runtests.py" "-vv" "--no-build" "--mode=fast")))))))
    (inputs
     (map specification->package
          (list "bash" "openblas")))
    (native-inputs (list python2-nose))
    (home-page "https://numpy.org")
    (synopsis "Fundamental package for scientific computing with Python")
    (description "NumPy is the fundamental package for scientific computing
with Python.  It contains among other things: a powerful N-dimensional array
object, sophisticated (broadcasting) functions, tools for integrating C/C++
and Fortran code, useful linear algebra, Fourier transform, and random number
capabilities.  Version 1.8 is the last one to contain the numpy.oldnumeric API
that includes the compatibility layer numpy.oldnumeric with NumPy's predecessor
Numeric.")
    (license license:bsd-3)))

;; Pandas 0.24.x are the last versions that support Python 2.
(define-python2-package python2-pandas
  (package
    (name "python2-pandas")
    (version "0.24.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pandas" version))
              (sha256
               (base32 "18imlm8xbhcbwy4wa957a1fkamrcb0z988z006jpfda3ki09z4ag"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Adjust for renamed error message in Python 2.7.17.  Taken
                  ;; from <https://github.com/pandas-dev/pandas/pull/29294>.
                  (substitute* "pandas/io/parsers.py"
                    (("if 'NULL byte' in msg:")
                     "if 'NULL byte' in msg or 'line contains NUL' in msg:"))
                  ;; Remove the pre-cythonized files.
                  (with-directory-excursion "pandas"
                    (for-each delete-file
                              (list "io/sas/sas.c"
                                    "io/msgpack/_unpacker.cpp"
                                    "io/msgpack/_packer.cpp"
                                    "_libs/writers.c"
                                    "_libs/window.cpp"
                                    "_libs/tslibs/timezones.c"
                                    "_libs/tslibs/timestamps.c"
                                    "_libs/tslibs/timedeltas.c"
                                    "_libs/tslibs/strptime.c"
                                    "_libs/tslibs/resolution.c"
                                    "_libs/tslibs/period.c"
                                    "_libs/tslibs/parsing.c"
                                    "_libs/tslibs/offsets.c"
                                    "_libs/tslibs/np_datetime.c"
                                    "_libs/tslibs/nattype.c"
                                    "_libs/tslibs/frequencies.c"
                                    "_libs/tslibs/fields.c"
                                    "_libs/tslibs/conversion.c"
                                    "_libs/tslibs/ccalendar.c"
                                    "_libs/tslib.c"
                                    "_libs/testing.c"
                                    "_libs/sparse.c"
                                    "_libs/skiplist.c"
                                    "_libs/reshape.c"
                                    "_libs/reduction.c"
                                    "_libs/properties.c"
                                    "_libs/parsers.c"
                                    "_libs/ops.c"
                                    "_libs/missing.c"
                                    "_libs/lib.c"
                                    "_libs/join.c"
                                    "_libs/interval.c"
                                    "_libs/internals.c"
                                    "_libs/indexing.c"
                                    "_libs/index.c"
                                    "_libs/hashtable.c"
                                    "_libs/hashing.c"
                                    "_libs/groupby.c"
                                    "_libs/algos.c")))))))
    (build-system python-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (guix build python-build-system)
                  (ice-9 ftw)
                  (srfi srfi-26))
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-which
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((which (assoc-ref inputs "which")))
               (substitute* "pandas/io/clipboard/__init__.py"
                 (("^CHECK_CMD = .*")
                  (string-append "CHECK_CMD = \"" which "\"\n"))))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (let ((build-directory
                    (string-append
                     (getcwd) "/build/"
                     (car (scandir "build"
                                   (cut string-prefix? "lib." <>))))))
               (when tests?
                 ;; Disable the "strict data files" option which causes
                 ;; the build to error out if required data files are
                 ;; not available (as is the case with PyPI archives).
                 (substitute* "setup.cfg"
                   (("addopts = --strict-data-files") "addopts = "))
                 (with-directory-excursion build-directory
                   ;; Delete tests that require "moto" which is not yet
                   ;; in Guix.
                   (for-each delete-file
                             '("pandas/tests/io/conftest.py"
                               "pandas/tests/io/json/test_compression.py"
                               "pandas/tests/io/parser/test_network.py"
                               "pandas/tests/io/test_parquet.py"))
                   (invoke "pytest" "-vv" "pandas" "--skip-slow"
                           "--skip-network" "-k"
                           ;; XXX: Due to the deleted tests above.
                           "not test_read_s3_jsonl")))))))))
    (propagated-inputs
     (map S2 (list "python-dateutil"
                   "python-numpy"
                   "python-openpyxl"
                   "python-pytz"
                   "python-xlrd")))
    (inputs
     (list (S "which")))
    (native-inputs
     (map S2 (list "python-cython"
                   ;; For the tests
                   "python-beautifulsoup4"
                   "python-html5lib"
                   "python-hypothesis"
                   "python-lxml"
                   "python-nose"
                   "python-pytest"
                   "python-pytest-mock")))
    (home-page "https://pandas.pydata.org")
    (synopsis "Data structures for data analysis, time series, and statistics")
    (description
     "Pandas is a Python package providing fast, flexible, and expressive data
structures designed to make working with structured (tabular,
multidimensional, potentially heterogeneous) and time series data both easy
and intuitive.  It aims to be the fundamental high-level building block for
doing practical, real world data analysis in Python.")
    (license license:bsd-3)))

(define-python2-package python2-beautifulsoup4
  (package
    (name "python2-beautifulsoup4")
    (version "4.9.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "beautifulsoup4" version))
              (sha256
               (base32 "09gbd49mwz86k572r1231x2rdp82p42zlnw0bz9b9mfi58r9wwl4"))))
    (build-system python-build-system)
    (arguments
     (list #:python python-2))
    (propagated-inputs
     (map S2 (list "python-soupsieve" "python-html5lib" "python-lxml")))
    (home-page "https://www.crummy.com/software/BeautifulSoup/bs4/")
    (synopsis "Screen-scraping library")
    (description
     "Beautiful Soup is a Python library designed for rapidly setting up
screen-scraping projects.  It offers Pythonic idioms for navigating,
searching, and modifying a parse tree, providing a toolkit for
dissecting a document and extracting what you need.  It automatically
converts incoming documents to Unicode and outgoing documents to UTF-8.")
    (license license:expat)))

(define-python2-package python2-lxml
  (package
    (name "python2-lxml")
    (version "4.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lxml" version))
       (sha256
        (base32 "1jn4gbd1qxrbdli6x0bihi5rkm1h4qmfxqny9pa90bx8qgnfv13f"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            ;; Remove the pre-cythonized files.
            (with-directory-excursion "src/lxml"
              (for-each delete-file
                        (list "sax.c"
                              "objectify.c"
                              "lxml.etree_api.h"
                              "lxml.etree.h"
                              "html/diff.c"
                              "html/clean.c"
                              "etree_api.h"
                              "etree.h"
                              "etree.c"
                              "builder.c"
                              "_elementpath.c")))))))
    (build-system python-build-system)
    (arguments
     (list
       #:python python-2
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "make" "test")))))))
    (inputs
     (list (S "libxml2")
           (S "libxslt")))
    (native-inputs
     (list (S2 "python-cython")))
    (home-page "https://lxml.de/")
    (synopsis "Python XML processing library")
    (description
     "The lxml XML toolkit is a Pythonic binding for the C libraries
libxml2 and libxslt.")
    (license license:bsd-3))) ; and a few more, see LICENSES.txt

(define-python2-package python2-openpyxl
  (package
    (name "python2-openpyxl")
    (version "2.6.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "openpyxl" version))
              (sha256
               (base32 "1qzjj8nwj4dn0mhq1j64f136afiqqb81lvqiikipz3g1g0b80lqx"))))
    (build-system python-build-system)
    (arguments
     (list #:python python-2
           #:tests? #f))            ; No tests.
    (propagated-inputs
     (map S2 (list "python-et-xmlfile" "python-jdcal")))
    (home-page "https://openpyxl.readthedocs.io")
    (synopsis "Python library to read/write Excel 2010 XLSX/XLSM files")
    (description
     "This Python library allows reading and writing to the Excel XLSX, XLSM,
XLTX and XLTM file formats that are defined by the Office Open XML (OOXML)
standard.")
    (license license:expat)))

(define-python2-package python2-soupsieve
  (package
    (name "python2-soupsieve")
    (version "1.9.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "soupsieve" version))
              (sha256
               (base32 "0in9rc9q3h8w5b4qf7kvl3qxcvw6vrz35ckblchgf70hm6pg3dbj"))))
    (build-system python-build-system)
    (arguments
     (list #:python python-2
           #:tests? #f))    ; Prevent a circular dependency on beautifulsoup4.
    (propagated-inputs
     (list python2-backports-functools-lru-cache))
    (home-page "https://github.com/facelessuser/soupsieve")
    (synopsis "CSS selector library")
    (description
     "Soup Sieve is a CSS selector library designed to be used with Beautiful
Soup 4.  It aims to provide selecting, matching, and filtering using modern
CSS selectors.  Soup Sieve currently provides selectors from the CSS level 1
specifications up through the latest CSS level 4 drafts and beyond (though
some are not yet implemented).")
    (license license:expat)))

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
     `(#:python ,python-2
       #:tests? #false            ;FIXME: One of the tests is failing.
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

(define-python2-package python2-backports-functools-lru-cache
  (package
    (name "python2-backports-functools-lru-cache")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       ;; only the pypi tarballs contain the necessary metadata
       (uri (pypi-uri "backports.functools_lru_cache" version))
       (sha256
        (base32
         "0jidrkk2w6bhjm197plxiaxrav64mgcrign0bfyr7md2ilc5zplg"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (native-inputs
     (list (S2 "python-setuptools-scm")))
    (home-page "https://github.com/jaraco/backports.functools_lru_cache")
    (synopsis "Backport of functools.lru_cache from Python 3.3")
    (description "@code{python2-backports-functools-lru-cache} is a backport of
@code{functools.lru_cache} from Python 3.3.")
    (license license:expat)))

(define-python2-package python2-functools32
  (package
    (name "python2-functools32")
    (version "3.2.3-2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "functools32" version))
       (sha256
        (base32
         "0v8ya0b58x47wp216n1zamimv4iw57cxz3xxhzix52jkw3xks9gn"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f))                    ; no test target
    (home-page "https://github.com/MiCHiLU/python-functools32")
    (synopsis
     "Backport of the functools module from Python 3.2.3")
    (description
     "This package is a backport of the @code{functools} module from Python
3.2.3 for use with older versions of Python and PyPy.")
    (license license:expat)))

(define-python2-package python2-singledispatch
  (package
    (name "python2-singledispatch")
    (version "3.4.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "singledispatch" version))
       (sha256
        (base32
         "171b7ip0hsq5qm83np40h3phlr36ym18w0lay0a8v08kvy3sy1jv"))))
    (build-system python-build-system)
    (arguments
     (list #:python python-2))
    (native-inputs
     (list python2-six))
    (home-page
     "https://docs.python.org/3/library/functools.html#functools.singledispatch")
    (synopsis "Backport of singledispatch feature from Python 3.4")
    (description
     "This library brings functools.singledispatch from Python 3.4 to Python
2.6-3.3.")
    (license license:expat)))

(define-python2-package python2-pygobject-2
  (package
    (name "python2-pygobject")
    ;; This was the last version to declare the 2.0 platform number, i.e. its
    ;; pkg-config files were named pygobject-2.0.pc
    (version "2.28.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/pygobject/"
                           (version-major+minor version)
                           "/pygobject-" version ".tar.xz"))
       (sha256
        (base32
         "0nkam61rsn7y3wik3vw46wk5q2cjfh2iph57hl9m39rc8jijb7dv"))
       (patches (search-patches "past/patches/python2-pygobject-2-deprecation.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;segfaults during tests
       #:configure-flags '("LIBS=-lcairo-gobject")))
    (native-inputs
     (list (S "which")
           `(,(S "glib") "bin")       ;for tests: glib-compile-schemas
           (S "pkg-config")
           (S "dbus"))) ;for tests
    (inputs
     (list python-2
           (S "glib")
           (S2 "python-pycairo")
           (S "gobject-introspection")))
    (propagated-inputs
     (list (S "libffi")))                     ;mentioned in pygobject-2.0.pc
    (home-page "https://pypi.org/project/PyGObject/")
    (synopsis "Python bindings for GObject")
    (description
     "Python bindings for GLib, GObject, and GIO.")
    (license license:lgpl2.1+)))

(define-python2-package python2-subprocess32
  (package
    (name "python2-subprocess32")
    (version "3.2.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "subprocess32" version))
              (sha256
               (base32
                "14350dhhlhyz5gqzi3lihn9m6lvskx5mcb20srx1kgsk9i50li8y"))
              (patches
               (search-patches "past/patches/python2-subprocess32-disable-input-test.patch"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       ;; The test suite fails with Python > 2.7.13:
       ;;     import test.support
       ;; ImportError: No module named support
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh
           (lambda _
             (substitute* '("subprocess32.py"
                            "test_subprocess32.py")
               (("/bin/sh") (which "sh"))))))))
    (home-page "https://github.com/google/python-subprocess32")
    (synopsis "Backport of the subprocess module from Python 3.2")
    (description
     "This is a backport of the @code{subprocess} standard library module
from Python 3.2 and 3.3 for use on Python 2.  It includes bugfixes and some
new features.  On POSIX systems it is guaranteed to be reliable when used
in threaded applications.  It includes timeout support from Python 3.3 but
otherwise matches 3.2’s API.")
    (license license:psfl)))

;; Pycairo no longer supports Python 2 since version 1.19.0, so we stick
;; with this older version here.
(define-python2-package python2-pycairo
  (package
    (name "python2-pycairo")
    (version "1.18.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/pygobject/pycairo/releases"
                                  "/download/v" version "/pycairo-" version ".tar.gz"))
              (sha256
               (base32
                "0cb5n4r4nl0k1g90b1gz9iyk4lp7hi03db98i1p52a870bym7f6w"))))
    (build-system python-build-system)
    (arguments
     (list #:python python-2))
    (native-inputs
     (list (S "pkg-config")
           (S2 "python-pytest")))
    (propagated-inputs                  ;pycairo.pc references cairo
     (list (S "cairo")))
    (home-page "https://cairographics.org/pycairo/")
    (synopsis "Python bindings for cairo")
    (description
     "Pycairo is a set of Python bindings for the Cairo graphics library.")
    ;; Dual-licensed under LGPL 2.1 or Mozilla Public License 1.1
    (license (list license:lgpl2.1 license:mpl1.1))))

(define-python2-package python2-dateutil
  (package
    (name "python2-dateutil")
    (version "2.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-dateutil" version))
       (sha256
        (base32
         "11iy7m4bp2lgfkcl0r6xzf34bvk7ppjmsyn2ygfikbi72v6cl8q1"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             ;; Delete tests that depend on "freezegun" to avoid a
             ;; circular dependency.
             (delete-file "dateutil/test/test_utils.py")
             (delete-file "dateutil/test/test_rrule.py")

             ;; XXX: Fails to get timezone from /etc/localtime.
             (delete-file "dateutil/test/test_tz.py")

             (invoke "pytest" "-vv"))))))
    (native-inputs
     (map S2 (list "python-pytest"
                   "python-pytest-cov"
                   "python-hypothesis"
                   "python-setuptools-scm")))
    (propagated-inputs
     (list (S2 "python-six")))
    (home-page "https://dateutil.readthedocs.io/en/stable/")
    (synopsis "Extensions to the standard datetime module")
    (description
     "The dateutil module provides powerful extensions to the standard
datetime module, available in Python 2.3+.")
    ;; The license was changed from the three-clause BSD license to a dual
    ;; Apache 2.0/BSD-3 variant at 2017-12-01.  Some code is only available as
    ;; BSD-3 still; but all new code is dual licensed (the user can choose).
    (license (list license:bsd-3 license:asl2.0))))

(define-python2-package python2-matplotlib
  (package
    (name "python2-matplotlib")
    (version "2.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "matplotlib" version))
       (sha256
        (base32
         "1sk05fdai9rw35l983rw2ymvz0nafs7szs7yz4nxrpyr1j27l0x3"))))
    (build-system python-build-system)
    (arguments
     (list
      #:python python-2
      #:tests? #false                  ;tests were not run in the past
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'pretend-version
            ;; The version string is usually derived via setuptools-scm, but
            ;; without the git metadata available, the version string is set to
            ;; '0.0.0'.
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (add-before 'build 'configure-environment
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Fix rounding errors when using the x87 FPU.
              (when (string-prefix? "i686" #$(%current-system))
                (setenv "CFLAGS" "-ffloat-store"))
              (call-with-output-file "mplsetup.cfg"
                (lambda (port)
                  (format port "\
[libs]
system_freetype = true
system_qhull = true

[rc_options]
backend=Agg

[directories]
basedirlist = ~a,~a

[packages]
tests = True~%" (assoc-ref inputs "tcl") (assoc-ref inputs "tk"))))))
          (add-after 'install 'create-init-file
            (lambda _
              (with-output-to-file
                  (string-append
                   #$output
                   "/lib/python2.7/site-packages/mpl_toolkits/__init__.py")
                (lambda _ (display ""))))))))
    (native-inputs
     (map S (list "pkg-config")))
    (inputs
     (map S (list "cairo"
                  "freetype"
                  "glib"
                  "libpng"
                  "qhull"
                  "tcl"
                  "tk")))
    (propagated-inputs
     (cons (list python-2 "tk")
           (map S2 (list "gobject-introspection"
                         "python-backports-functools-lru-cache"
                         "python-certifi"
                         "python-cycler"
                         "python-dateutil"
                         "python-functools32"
                         "python-kiwisolver"
                         "python-numpy"
                         "python-pillow"
                         "python-pycairo"
                         "python-pygobject-2"
                         "python-pyparsing"
                         "python-pytz"
                         "python-six"
                         "python-subprocess32"))))
    (home-page "https://matplotlib.org/")
    (synopsis "2D plotting library for Python")
    (description
     "Matplotlib is a Python 2D plotting library which produces publication
quality figures in a variety of hardcopy formats and interactive environments
across platforms.  Matplotlib can be used in Python scripts, the python and
ipython shell, web application servers, and six graphical user interface
toolkits.")
    (license license:psfl)))

;; Version 1.2.x is the last version to support Python 2
(define-python2-package python2-scipy
  (package
    (name "python2-scipy")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "scipy" version))
              (sha256
               (base32
                "0myaghgjf5pqlxzx20669sq827h0mh1s7gvmhjgy3f4hr89n9gpc"))
              (snippet
               #~(begin
                   (use-modules (guix build utils))
                   ;; Remove the pre-cythonized files.
                   (with-directory-excursion "scipy"
                     (for-each delete-file
                               (list "cluster/_hierarchy.c"
                                     "cluster/_optimal_leaf_ordering.c"
                                     "cluster/_vq.c"
                                     "interpolate/_bspl.c"
                                     "interpolate/interpnd.c"
                                     "interpolate/_ppoly.c"
                                     "io/matlab/mio5_utils.c"
                                     "io/matlab/mio_utils.c"
                                     "io/matlab/streams.c"
                                     "_lib/_ccallback_c.c"
                                     "_lib/messagestream.c"
                                     "linalg/cython_blas.c"
                                     "linalg/cython_lapack.c"
                                     "linalg/_decomp_update.c"
                                     "linalg/_solve_toeplitz.c"
                                     "ndimage/src/_cytest.c"
                                     "ndimage/src/_ni_label.c"
                                     "optimize/_group_columns.c"
                                     "optimize/_lsq/givens_elimination.c"
                                     "optimize/_trlib/_trlib.c"
                                     "signal/_max_len_seq_inner.c"
                                     "signal/_peak_finding_utils.c"
                                     "signal/_spectral.c"
                                     "signal/_upfirdn_apply.c"
                                     "sparse/csgraph/_min_spanning_tree.c"
                                     "sparse/csgraph/_reordering.c"
                                     "sparse/csgraph/_shortest_path.c"
                                     "sparse/csgraph/_tools.c"
                                     "sparse/csgraph/_traversal.c"
                                     "sparse/_csparsetools.c"
                                     "spatial/ckdtree.cxx"
                                     "spatial/ckdtree.h"
                                     "spatial/_hausdorff.c"
                                     "spatial/qhull.c"
                                     "spatial/_voronoi.c"
                                     "special/_comb.c"
                                     "special/cython_special.c"
                                     "special/cython_special.h"
                                     "special/_ellip_harm_2.c"
                                     "special/_ellip_harm_2.h"
                                     "special/_test_round.c"
                                     "special/_ufuncs.c"
                                     "special/_ufuncs_cxx.cxx"
                                     "special/_ufuncs_cxx.h"
                                     "special/_ufuncs.h"
                                     "stats/_stats.c")))))))
    (build-system python-build-system)
    (arguments
     (list
      #:python python-2
      #:modules '((guix build utils)
                  (guix build python-build-system)
                  (ice-9 format))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              ;; test_curvefit_covariance: No idea why it fails, but
              ;; there's a very small difference in results.
              (substitute* "scipy/optimize/tests/test_minpack.py"
                (("import numpy as np" m)
                 (string-append "from pytest import mark as mark\n" m))
                (("^( +)def test_curvefit_covariance" m indent)
                 (string-append indent "@mark.skipif(True, reason=\"whatever\")\n" m)))
              ;; test_lapack: False positive, because two instances of
              ;; libgfortran end up in the list of dependencies.  The
              ;; test wants to guard against using f77 and gfortran.
              (substitute* "scipy/linalg/tests/test_build.py"
                (("^( +)def test_lapack" m indent)
                 (string-append indent "@pytest.mark.skipif(True, reason=\"Guix\")\n" m)))))
          (add-after 'unpack 'disable-pythran
            (lambda _
              (setenv "SCIPY_USE_PYTHRAN" "0")))
          (add-before 'build 'cythonize-files
            (lambda _
              (invoke "python" "tools/cythonize.py")))
          (add-before 'build 'change-home-dir
            (lambda _
              ;; Change from /homeless-shelter to /tmp for write permission.
              (setenv "HOME" "/tmp")))
          (add-before 'build 'configure-openblas
            (lambda _
              (call-with-output-file "site.cfg"
                (lambda (port)
                  (format port
                          "\
[blas]
libraries = openblas
library_dirs = ~a/lib
include_dirs = ~:*~a/include

[atlas]
library_dirs = ~:*~a/lib
atlas_libs = openblas~%"  #$(this-package-input "openblas"))))))
          (add-before 'build 'parallelize-build
            (lambda _
              (setenv "NPY_NUM_BUILD_JOBS"
                      (number->string (parallel-job-count)))))
          (replace 'build
            (lambda _
              (invoke "python" "setup.py" "build"
                      "--fcompiler=gnu95"
                      "-j" (number->string (parallel-job-count)))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "./runtests.py" "-vv" "--no-build" "--mode=fast"
                        "-j" (number->string (parallel-job-count)))))))))
    (propagated-inputs
     (list (S2 "python-numpy")
           (S2 "python-matplotlib")
           (S2 "python-pyparsing")))
    (inputs
     (list (S "openblas")
           (S2 "pybind11")))
    (native-inputs
     (cons* gfortran-7
            gcc-7
            (list (S2 "python-cython")
                  (S2 "python-pytest")
                  (S "perl")
                  (S "which"))))
    (home-page "https://scipy.org/")
    (synopsis "The Scipy library provides efficient numerical routines")
    (description "The SciPy library is one of the core packages that make up
the SciPy stack.  It provides many user-friendly and efficient numerical
routines such as routines for numerical integration and optimization.")
    (license license:bsd-3)))

(define-python2-package python2-pyro
  (package
    (name "python2-pyro")
    (version "3.16")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Pyro" version))
       (file-name (string-append "Pyro-" version ".tar.gz"))
       (sha256
        (base32
         "0y75wzdqbjy565rpxaxscav4j8xg060sa90lnmb7aypgaf251v8v"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; Pyro has no test cases for automatic execution
       #:python ,python-2))
    (home-page "https://pythonhosted.org/Pyro/")
    (synopsis "Distributed object manager for Python")
    (description "Pyro is a Distributed Object Technology system
written in Python that is designed to be easy to use.  It resembles
Java's Remote Method Invocation (RMI).  It has less similarity to CORBA,
which is a system and language independent Distributed Object Technology
and has much more to offer than Pyro or RMI.  Pyro 3.x is no
longer maintained.  New projects should use Pyro4 instead, which
is the new Pyro version that is actively developed.")
    (license license:expat)))

(define-python2-package python2-scientific
  (package
    (name "python2-scientific")
    (version "2.9.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ScientificPython" version))
       (sha256
        (base32
         "14dncpnmv4n9rj974ay9k7rq06ifcq7pdw8xg6rk71v0k7f465p4"))))
    (build-system python-build-system)
    (inputs
     (list (S "netcdf")))
    (propagated-inputs
     (list python2-numpy-1.8 python2-pyro))
    (arguments
     ;; ScientificPython is not compatible with Python 3
     `(#:python ,python-2
       #:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "python" "setup.py" "build"
                     (string-append "--netcdf_prefix="
                                    (assoc-ref inputs "netcdf"))))))))
    (home-page "http://dirac.cnrs-orleans.fr/ScientificPython")
    (synopsis "Python modules for scientific computing")
    (description "ScientificPython is a collection of Python modules that are
useful for scientific computing.  Most modules are rather general (Geometry,
physical units, automatic derivatives, ...) whereas others are more
domain-specific (e.g. netCDF and PDB support).  The library is currently
not actively maintained and works only with Python 2 and NumPy < 1.9.")
    (license license:cecill-c)))

(define-python2-package python2-mmtk
  (package
    (name "python2-mmtk")
    (version "2.7.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/khinsen/MMTK")
             (commit (string-append "rel" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1fqwh3ba9jd42nigvn5shndgwb1zy7kh9520ncvqci7n8ffjr6p1"))))
    (build-system python-build-system)
    (native-inputs
     (list netcdf))
    (propagated-inputs
     `(("python-scientific" ,python2-scientific)
       ("python-tkinter" ,python-2 "tk")))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'includes-from-scientific
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "Include/Scientific")
             (copy-recursively
                     (string-append
                      (assoc-ref inputs "python-scientific")
                      "/include/python2.7/Scientific")
                     "Include/Scientific")))
         (replace 'check
           (lambda* (#:key (tests? #t) #:allow-other-keys)
             (when tests?
               (with-directory-excursion "Tests"
                   (invoke "python" "all_tests.py"))))))))
    (home-page "http://dirac.cnrs-orleans.fr/MMTK")
    (synopsis "Python library for molecular simulation")
    (description "MMTK is a library for molecular simulations with an emphasis
on biomolecules.  It provides widely used methods such as Molecular Dynamics
and normal mode analysis, but also basic routines for implementing new methods
for simulation and analysis.  The library is currently not actively maintained
and works only with Python 2 and NumPy < 1.9.")
    (license license:cecill-c)))
