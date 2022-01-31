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
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
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

(define-public qhull-3.0
  (package
    (inherit qhull)
    (version "3.0")
    (source (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/qhull/qhull.git")
            (commit "v3.0")))
      (file-name (git-file-name "qhull" version))
      (sha256
       (base32
        "05wm1b8c9lnmq3dvpb4q6g9168v5irvy69qnwiwmvm3faamfhw75"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; no check phase
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (chdir "src")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (program)
                           (install-file program bin))
                         '("qconvex" "qdelaunay" "qhalf" "qhull"
                           "qvoronoi")))
               #t)))))))

(define-public primal-dual
  (package
    (name "primal-dual")
    (version "0.97.11.20") ; no version, use the release date
    (source (origin
      (method url-fetch/tarbomb)
      (uri "http://www.cs.unb.ca/~bremner/software/pd/pd.tar.gz")
      (sha256
       (base32
        "04qhgbviv2f7iblhamjxzmapp1hyhalaa2q8a03f9xzc2phd102p"))
      ;; The makefile is hard-wired to 32 bit architectures; to simplify,
      ;; we patch it to work only in 64 bit mode, given that this is the
      ;; only known use in the guix-past channel.
      ;; Alternatively, one could patch the makefile in a build phase
      ;; depending on the architecture.
      (patches
       (search-patches "past/patches/primal-dual-64bit.patch"))))
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; no check phase
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "pd" bin))
             #t)))))
    (home-page "http://www.cs.unb.ca/~bremner/pd/")
    (synopsis "Primal-dual method for vertex and facet enumeration")
    (description "Primal-dual, or pd in short, implements an algorithm for
computing the vertex representation from the facet representation of a
convex polytope, and vice versa.  It uses one direction as an oracle for
the other one, so it is meant to work well when other algorithms face
a difficult direction (and should be avoided for the easy direction).")
    (license license:gpl3+)))

(define-public vinci-0.97.10.12
  (package
    (inherit vinci)
    (version "0.97.10.12") ; use the release date instead of the not very
                           ; descriptive "gamma"
    (source (origin
      (method url-fetch)
      (uri (string-append "https://gitlab.inria.fr/enge/revinci/-/raw/"
                          "master/code/vinci_gamma.tar.bz2"))
      (sha256
       (base32
        "0yd4mv05v86kbybrcr1ignhzv6msq9gx668r7rvc6k44b4r97xg7"))
      (patches
       (search-patches "past/patches/vinci-dummy.patch"
                       "past/patches/vinci-print.patch"
                       "past/patches/vinci-pivoting.patch"))))
    (inputs
     `(("lrslib" ,lrslib-4.0)))))
