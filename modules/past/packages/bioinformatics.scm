;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2021, 2022 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (past packages bioinformatics)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages statistics)
  #:use-module (past packages python27))

(define-public couger
  (package
    (name "couger")
    (version "1.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://couger.oit.duke.edu/static/assets/COUGER"
                    version ".zip"))
              (sha256
               (base32
                "04p2b14nmhzxw5h72mpzdhalv21bx4w9b87z0wpw0xzxpysyncmq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; there are none
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (copy-recursively "src" (string-append out "/src"))
               (mkdir bin)
               ;; Add "src" directory to module lookup path.
               (substitute* "couger"
                 (("from argparse")
                  (string-append "import sys\nsys.path.append(\""
                                 out "\")\nfrom argparse")))
               (install-file "couger" bin))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'couger' runs with the correct PYTHONPATH.
             (let* ((out (assoc-ref outputs "out"))
                    (path (getenv "PYTHONPATH")))
               (wrap-program (string-append out "/bin/couger")
                 `("PYTHONPATH" ":" prefix (,path)))))))))
    (inputs
     `(("python" ,python-2)
       ("python2-pillow" ,python2-pillow)
       ("python2-numpy" ,python2-numpy)
       ("python2-scipy" ,python2-scipy)
       ("python2-matplotlib" ,python2-matplotlib)))
    (propagated-inputs
     `(("r-minimal" ,r-minimal)
       ("libsvm" ,libsvm)
       ("randomjungle" ,randomjungle)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "http://couger.oit.duke.edu")
    (synopsis "Identify co-factors in sets of genomic regions")
    (description
     "COUGER can be applied to any two sets of genomic regions bound
by paralogous TFs (e.g., regions derived from ChIP-seq experiments) to
identify putative co-factors that provide specificity to each TF.  The
framework determines the genomic targets uniquely-bound by each TF,
and identifies a small set of co-factors that best explain the in vivo
binding differences between the two TFs.

COUGER uses classification algorithms (support vector machines and
random forests) with features that reflect the DNA binding
specificities of putative co-factors.  The features are generated
either from high-throughput TF-DNA binding data (from protein binding
microarray experiments), or from large collections of DNA motifs.")
    (license license:gpl3+)))
