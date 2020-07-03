;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (past packages maths)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages))

(define S specification->package)

(define-public gsl-1.16
  (package
    (name "gsl")
    (version "1.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gsl/gsl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0lrgipi0z6559jqh82yx8n4xgnxkhzj46v96dl77hahdp58jzg3k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; XXX: Both of these tests fail like this:
         ;; gsl: fprintf_source.c:164: ERROR: fscanf failed
         ;; Default GSL error handler invoked.
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             (substitute* "vector/Makefile.in"
               (("^(check_PROGRAMS = test\\$\\(EXEEXT\\)).*$" _ m)
                (string-append m "\n")))
             (substitute* "matrix/Makefile.in"
               (("^check_PROGRAMS = .*$")
                "check_PROGRAMS = test_static$(EXEEXT)\n"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,(S "pkg-config"))))
    (home-page "https://www.gnu.org/software/gsl/")
    (synopsis "Numerical library for C and C++")
    (description
     "The GNU Scientific Library is a library for numerical analysis
in C and C++.  It includes a wide range of mathematical routines, with
over 1000 functions in total.  Subject areas covered by the library
include: differential equations, linear algebra, Fast Fourier
Transforms and random numbers.")
    (license license:gpl3+)))
