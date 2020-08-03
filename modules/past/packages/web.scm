;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (past packages web)
  #:use-module (guix)
  #:use-module (guix download)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (srfi srfi-1))

(define-public httpd-2.2
  (package
    (inherit httpd)
    (version "2.2.34")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://apache/httpd/httpd-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0q4196krxbyaswl5yzmm0r5ry2dijfhvxjdld3bl0mxcvzaq6cg5"))))
    (inputs
     `(("openssl" ,openssl-1.0)
       ,@(alist-delete "openssl" (package-inputs httpd))))
    (arguments
     (substitute-keyword-arguments (package-arguments httpd)
       ((#:configure-flags flags)
        `(cons "--enable-mods-shared=most" ,flags))))
    (properties '((release-date "2017-07-24")))))
