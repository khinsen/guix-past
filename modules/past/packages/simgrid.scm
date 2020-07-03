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

(define-module (past packages simgrid)
  #:use-module (guix)
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
     '(#:tests? #f))
    (home-page "https://simgrid.org")
    (synopsis "Distributed system simulator (2009 version)")
    (description
     "SimGrid is a scientific instrument to study the behavior of large-scale
distributed systems such as grids, \"clouds\", HPC, and P2P systems.  It can
be used to evaluate heuristics, prototype applications or even assess legacy
MPI applications.")
    (license license:lgpl2.1+)))
