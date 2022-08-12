;;; Guix Past --- Packages from the past for GNU Guix.
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

(define-module (past packages chemistry)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages python)
  #:use-module (past packages python27))

(define-public domainfinder
  (package
    (name "domainfinder")
    (version "2.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/khinsen/DomainFinder")
             (commit (string-append "rel" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1c5g10ap3xhix5alp9fnq4f2bgvpyy6k409rac0h0icpgahxj8kg"))))
    (build-system python-build-system)
    (inputs
     (list python2-mmtk))
    (arguments
     `(#:python ,python-2
       ;; No test suite
       #:tests? #f))
    (home-page "http://dirac.cnrs-orleans.fr/DomainFinder.html")
    (synopsis "Analysis of dynamical domains in proteins")
    (description "DomainFinder is an interactive program for the determination
and characterization of dynamical domains in proteins.  It can infer dynamical
domains by comparing two protein structures, or from normal mode analysis on a
single structure.  The software is currently not actively maintained and works
only with Python 2 and NumPy < 1.9.")
    (license license:cecill-c)))
