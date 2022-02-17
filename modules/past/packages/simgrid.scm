;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2020-2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (past packages simgrid)
  #:use-module (guix)
  #:use-module (gnu packages)
  #:use-module (guix svn-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public simgrid-3.3
  (package
    (name "simgrid")
    (version "3.3")                               ;from 2009-04-12
    (source (origin
              (method url-fetch)
              (uri
               ;; gforge.inria.fr will be taken down in Dec. 2020, but the
               ;; tarball is archived at
               ;; <https://archive.softwareheritage.org/api/1/content/sha256:e5cb6128f602975009b5ceac2f3a35797e99cf3d39de5d8dbf963d0855f94d9f/>.
               "https://gforge.inria.fr/frs/download.php/file/21430/simgrid-3.3.tar.gz")
              (sha256
               (base32
                "17sdz5ahhgcnpy6mvpir7p7rjzkr6lx2zb6fnl4m15q2yql63jz5"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: There are test failures of various kinds.
     '(#:tests? #f

       ;; Work around compilation error when using GCC 10+.
       #:configure-flags '("CFLAGS=-O2 -g -fcommon")))
    (home-page "https://simgrid.org")
    (synopsis "Distributed system simulator (2009 version)")
    (description
     "SimGrid is a scientific instrument to study the behavior of large-scale
distributed systems such as grids, \"clouds\", HPC, and P2P systems.  It can
be used to evaluate heuristics, prototype applications or even assess legacy
MPI applications.")
    (license license:lgpl2.1+)))


;;; What follows is a 2009 variant of GTNetS tailored for the purposes of
;;; <https://github.com/ReScience/submissions/issues/39>.

(define S specification->package)

(define gtnets-source
  ;; This Subversion checkout contains a zip file, the GTNetS source, along
  ;; with a tarball containing patches.
  ;;
  ;; However, gforge.inria.fr was shut down in Dec. 2021.  Thus, instead,
  ;; arrange to fetch the individual files from Software Heritage (see
  ;; below).  SWH also has a copy of the whole checkout at
  ;; <https://archive.softwareheritage.org/browse/origin/directory/?origin_url=svn://scm.gforge.inria.fr/svn/simgrid/contrib/trunk/GTNetS>.
  (origin
    (method svn-fetch)
    (uri (svn-reference
          (url "svn://scm.gforge.inria.fr/svn/simgrid/contrib/trunk/GTNetS/")
          (revision 7017)))
    (file-name "gtnets-simgrid-checkout")
    (sha256
     (base32
      "1inll6z4fvafcr1jh8knpswpxzzfdqbdxdplib8ivxvfwxp992xw"))))

(define gtnets-zipball
  ;; The zip file containing the original gtnets source.  Here we make it
  ;; content-addressed (with #:hash) so it can be downloaded from Software
  ;; Heritage as a fallback.  (This file used to be extracted from
  ;; GTNETS-SOURCE above.)
  (origin
    (method url-fetch)
    (uri "http://example.org/placeholder")        ;force fallback to SWH
    (file-name "gtnets-current.zip")
    (sha256
     (base32 "1l06aalr925xnzxrlz3f3gc40hs6ac9w0z21vqyr9fljs71301h8"))))


(define gtnets-patch-tarball
  ;; Tarball containing patches.
  (origin
    (method url-fetch)
    (uri "http://example.org/placeholder")        ;force fallback to SWH
    (file-name "gtnets-current-patch.tgz")
    (sha256
     (base32 "16p8vrzhy1266l4pfyahcw4iccg3b9izdh3qp2fwh33kwkyjkcz3"))))

(define-public gtnets
  (package
    (name "gtnets")
    (version "0")
    (source gtnets-zipball)
    (build-system gnu-build-system)
    (arguments
     ;; Here we roughly follow the build instructions from the Dockerfile in
     ;; <https://github.com/ReScience/submissions/issues/39>.
     '(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'apply-patches
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; Extract the patches
                      (let ((patches (assoc-ref inputs "patches")))
                        (invoke "tar" "xvf" patches)
                        (for-each (lambda (patch)
                                    (invoke "patch" "-p1" "--batch"
                                            "-i" patch))
                                  (find-files "." "\\.patch$"))
                        #t)))
                  (replace 'configure
                    (lambda _
                      (delete-file "Makefile")
                      (symlink "Makefile.linux" "Makefile")
                      (substitute* "Makefile"
                        (("g\\+\\+")
                         "g++ -std=c++98 -fpermissive"))
                      #t))
                  (replace 'build
                    (lambda _
                      ;; Build the library only (there's a GUI requiring Qt
                      ;; that we don't build.)
                      (invoke "make" "libgtsim-opt.so" "-j"
                              (number->string (parallel-job-count)))))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Install the library and header.
                      (let* ((out (assoc-ref outputs "out"))
                             (lib (string-append out "/lib"))
                             (include (string-append out "/include")))
                        (mkdir-p lib)
                        (copy-file "libgtsim-opt.so"
                                   (string-append lib "/libgtnets.so"))
                        (for-each (lambda (header)
                                    (install-file header include))
                                  (find-files "SRC" "\\.h$"))
                        #t))))
       #:tests? #f))                              ;there are no tests
    (native-inputs
     `(("unzip" ,(S "unzip"))
       ("patches" ,gtnets-patch-tarball)))
    (home-page "https://simgrid.org")
    (synopsis "Network simulation environment (2009 version)")
    (description
     "The Georgia Tech Network Simulator (GTNets) is designed to allow
network researchers to conduct simulation-based experiments to observe the
behavior of moderate to large scale computer networks under a variety of
conditions. The GTNets environment allows the creation of simulation network
topologies (consisting of nodes and their associated communication links),
and end–user applications describing the flow of data over the simulated
topology.")
    (license license:gpl2+)))
