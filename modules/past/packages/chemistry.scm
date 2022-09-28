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
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages onc-rpc)
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

(define with-numpy-1.8
  (package-input-rewriting `((,python2-numpy . ,python2-numpy-1.8))))

(define-public nmoldyn
  (package
    (name "nmoldyn")
    (version "3.0.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/khinsen/nMOLDYN3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "02b5i6il26kqg3dkrln7vcix8i3306ynsk776gzd5cwninhygdqh"))))
    (build-system python-build-system)
    (inputs
     (list (with-numpy-1.8 python2-matplotlib) python2-scientific
           (specification->package "gv")))
    (propagated-inputs
     (list python2-mmtk (specification->package "netcdf")))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-directory-case
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "nMOLDYN/Tests/AnalysisTests.py"
               (("from nMoldyn")
                "from nMOLDYN"))))
         (add-before 'build 'create-linux2-directory
           (lambda _
             (mkdir-p "nMOLDYN/linux2")))
         (add-before 'build 'change-PDF-viewer
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "nMOLDYN/Preferences.py"
               ;; Set the paths for external executables, substituting
               ;; gv for acroread.
               ;; There is also vmd_path, but VMD is not free software
               ;; and Guix contains currently no free molecular viewer that
               ;; could be substituted.
               (("PREFERENCES\\['acroread_path'\\] = ''")
                (format #f "PREFERENCES['acroread_path'] = '~a'"
                        (which "gv")))
               (("PREFERENCES\\['ncdump_path'\\] = ''")
                (format #f "PREFERENCES['ncdump_path'] = '~a'"
                        (which "ncdump")))
               (("PREFERENCES\\['ncgen_path'\\] = ''")
                (format #f "PREFERENCES['ncgen_path'] = '~a'"
                        (which "ncgen3")))
               (("PREFERENCES\\['task_manager_path'\\] = ''")
                (format #f "PREFERENCES['task_manager_path'] = '~a'"
                        (which "task_manager")))
               ;; Show documentation as PDF
               (("PREFERENCES\\['documentation_style'\\] = 'html'")
                "PREFERENCES['documentation_style'] = 'pdf'") )))
         (delete 'sanity-check)
         (replace 'check
           (lambda* (#:key (tests? #t) #:allow-other-keys)
             (when tests?
               (with-directory-excursion "nMOLDYN/Tests"
                   (invoke "python" "AnalysisTests.py"))))))))
    (home-page "http://dirac.cnrs-orleans.fr/nMOLDYN.html")
    (synopsis "Analysis software for Molecular Dynamics trajectories")
    (description "nMOLDYN is an interactive analysis program for Molecular Dynamics
simulations.  It is especially designed for the computation and decomposition of
neutron scattering spectra, but also computes other quantities.  The software
is currently not actively maintained and works only with Python 2 and
NumPy < 1.9.")
    (license license:cecill)))

(define-public python2-gromacstrajectoryreader
  (package
   (name "python2-gromacstrajectoryreader")
   (version "0.13")
   (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/khinsen/GromacsTrajectoryReader")
            (commit (string-append "v" version))))
     (sha256
      (base32
       "00k4ygrvji2kwlyaq8a57a4qlzd61bc38dcqjk6h0zkawfilmzjf"))))
   (build-system python-build-system)
   (propagated-inputs
    (list python2-numpy-1.8))
   (native-inputs
    (list python2-cython))
   (inputs
    (list libtirpc))
   (arguments
    `(#:python ,python-2
      #:phases (modify-phases %standard-phases
                  (add-before 'build 'set-libtirpc-include-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; Allow <rpc/rpc.h> & co. to be found.
                      (let ((tirpc (string-append (assoc-ref inputs "libtirpc")
                                                  "/include/tirpc")))
                        (if (getenv "CPATH")
                          (setenv "CPATH"
                                  (string-append (getenv "CPATH")
                                                 ":" tirpc))
                          (setenv "CPATH" tirpc))))))))
   (home-page "http://github.com/khinsen/GromacsTrajectoryReader")
   (synopsis "Reader for Molecular Dynamics trajectories in GROMACS format")
   (description "Reader for Molecular Dynamics trajectories in GROMACS format")
   (license license:cecill-c)))
