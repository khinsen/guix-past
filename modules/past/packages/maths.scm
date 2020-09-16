;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Andreas Enge <andreas@enge.fr>
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
  #:use-module (gnu packages)
  #:use-module (gnu packages maths))

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
    ;; With parallel testing fscan fails.
    (arguments '(#:parallel-tests? #f))
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

(define-public lrslib-4.0
  (package
    (inherit lrslib)
    (version "4.0")
    (source (origin
      (method url-fetch)
      (uri (string-append "http://cgm.cs.mcgill.ca/~avis/C/lrslib/archive/"
                          "lrslib-040.tar.gz"))
      (sha256
       (base32
        "1bgc46ihmp0yzhy1r74f1w9qk8zd8kr643xym9md8fzqdsa2lwy2"))
      (patches
       (search-patches "past/patches/lrs-getline.patch"))))
    (inputs '())
    (arguments
     `(#:tests? #f  ; no check phase
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (program)
                           (install-file program bin))
                         '("buffer" "lrs" "lrs1" "redund" "redund1")))
             #t)))))))
