;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
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

(define-module (past packages guile-xyz)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (past packages autotools))

(define-public guile-lib/guile-1.8
  (package
    (inherit guile-lib)
    (name "guile1.8-lib")
    (version "0.1.3")                             ;Nov. 2006
    (source (origin
              ;; Old source tarballs seem to have disappeared, except at
              ;; <http://snapshot.debian.org/package/guile-lib/> or
              ;; <https://web.archive.org/web/20041114175005/http://stud3.tuwien.ac.at/~e9926584/GuileLib>
              ;; for some of them.  Anyway, build from Git, because we can.
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.savannah.gnu.org/git/guile-lib.git")
                    (commit (string-append "release-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a9fmpclhfrjq28jdc2j10rdambls2178v2gzr0v5gbsd5wp17bv"))))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'bootstrap 'disable-configure
                    (lambda _
                      (setenv "NOCONFIGURE" "yes, well, no")
                      #t)))

       ;; XXX: Skip obscure test failures.
       #:tests? #f))
    (native-inputs
     `(("autoconf" ,autoconf-2.59)
       ("automake" ,automake-1.9)
       ("texinfo" ,texinfo-4)))
    (inputs
     `(("guile" ,guile-1.8)))
    (propagated-inputs '())))

(define-public g-wrap/guile-1.8
  (package
    (inherit g-wrap)
    (name "g-wrap-guile18")
    (version "1.9.7")                             ;Nov. 2006
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/g-wrap/old/g-wrap-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0mak8cgnyk1xs3xwkawvb6iwng7fpxg5kjs5mjxc99n51in0j7qr"))))
    (inputs '())
    (native-inputs '())
    (propagated-inputs
     `(("guile" ,guile-1.8)
       ("libffi" ,libffi)
       ;; G-Wrap needs Guile-Lib's (srfi srfi-35) implementation, which
       ;; provided GOOPS classes, as opposed to Guile's implementation.
       ;; It also needs its (unit-test) module for its own test suite.
       ("guile-lib" ,guile-lib/guile-1.8)))
    (arguments
     `(#:configure-flags '("--disable-Werror" "--disable-static")))))
