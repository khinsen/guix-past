;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (past packages graphviz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (srfi srfi-1))

(define-public graphviz-2.26
  (package
    (inherit graphviz)
    (name "graphviz")
    (version "2.26.3")
    (outputs '("out"))
    (source
      (origin
        (method url-fetch)
        (uri (list
               (string-append
                 "https://web.archive.org/web/20101223105832/"
                 "http://graphviz.org/pub/graphviz/stable/SOURCES/"
                 "graphviz-" version ".tar.gz")
               (string-append
                 "mirror://debian/pool/main/g/graphviz/"
                 "graphviz_" version ".orig.tar.gz")))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "18bzyg17ni0lpcd2g5dhan8fjv3vzkjym38jq8vm42did5p9j47l"))))
    (arguments
     `(#:configure-flags '("--enable-swig=no")

       ;; FIXME: rtest/rtest.sh is a ksh script (!).  Add ksh as an input.
       #:tests? #f))
    (inputs
     ;; TODO(?): Add language bindings.
     `(("gdk-pixbuf" ,gdk-pixbuf)
       ("freeglut" ,freeglut)
       ,@(fold alist-delete (package-inputs graphviz)
               '("libjpeg-turbo" "guile" "swig"))))
    (properties '((release-date . "2010-02-01")))
    (license license:cpl1.0)))
