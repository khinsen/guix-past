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

(define-module (past packages autotools)
  #:use-module (guix)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages perl))

(define-public autoconf-2.59                      ;Dec. 2003
  (package
    (inherit autoconf)
    (version "2.59")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/autoconf/autoconf-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1n489icp4p60lzjzlzz9ng3vk35v6sn6rpakmv6gbcgwqmrmrl4w"))))
    (arguments
     '(#:tests? #f))))

(define-public automake-1.9
  (package
    (inherit automake)
    (version "1.9.6")                             ;July 2005
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/automake/automake-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1f8kpbbf7nr3vx4bnxmzqmw3xk53ciwgg9ax0zi7x87psc6h7lz6"))))
    (native-inputs
     `(("perl" ,perl)
       ("autoconf" ,autoconf-2.59)))
    (inputs
     `(("guile" ,guile-2.2)))
    (propagated-inputs '())
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-aclocal
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (rename-file (string-append bin "/aclocal")
                            (string-append bin "/.aclocal-real"))
               (call-with-output-file (string-append bin "/aclocal")
                 (lambda (port)
                   (format port "#!~a --no-auto-compile~%!#~%"
                           (which "guile"))
                   (write '(use-modules (srfi srfi-1)) port)
                   (write `(let ((path (if (getenv "ACLOCAL_PATH")
                                           (string-split (getenv
                                                          "ACLOCAL_PATH")
                                                         #\:)
                                           '())))
                             (apply execl ,(string-append bin
                                                          "/.aclocal-real")
                                    (append (command-line)
                                            (append-map (lambda (directory)
                                                          `("-I" ,directory))
                                                        path))))
                          port)
                   (chmod port #o555)))
               #t))))))))

(define-public libtool-1.5
  (package
    (inherit libtool)
    (version "1.5.22")                            ;Dec. 2005
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/libtool/libtool-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0pgbihng0wp6nql9ljghmv61ijml5zfj6xr9yvd1s772mnmgzsc8"))))
    (arguments
     '(#:tests? #f))
    (inputs '())
    (native-inputs `(("m4" ,m4)
                     ("perl" ,perl)))
    (propagated-inputs '())))
