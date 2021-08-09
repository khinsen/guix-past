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
  #:use-module (guix build-system gnu)
  #:use-module (past packages python)
  #:use-module (past packages tls)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages tcl)
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
    (properties '((release-date . "2017-07-24")))))

;; httpd@2.2 and mod-python@3.3.1, both built with python@2.4, in the same prefix.
;; This is the only way to integrate mod-python with httpd.
(define-public httpd22-with-mod-python
  (package
    (inherit httpd-2.2)
    (name "httpd-with-mod-python")
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%gnu-build-system-modules)
       #:modules (((guix build gnu-build-system) #:prefix gnu:)
                  (guix build utils)
                  ,@%gnu-build-system-modules
                  ((guix build python-build-system) #:prefix python:))
       ,@(substitute-keyword-arguments (package-arguments httpd-2.2)
           ((#:phases phases '%standard-phases)
            `(modify-phases ,phases
               (add-after 'install 'unpack-mod-python
                 (lambda* args
                   ((assoc-ref gnu:%standard-phases 'unpack)
                    #:source (assoc-ref %build-inputs "mod-python"))))
               (add-after 'unpack-mod-python 'change-directory
                 (lambda _
                   ;; Make sure we're in the correct folder
                   (chdir "../mod_python-3.3.1")
                   #t))
               (add-after 'change-directory 'bootstrap-mod-python
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let* ((python (assoc-ref inputs "python"))
                          (tcl (assoc-ref inputs "tcl"))
                          (py-version (python:python-version python))
                          (tcl-version ,(version-major+minor (package-version tcl))))
                     (substitute* "configure.in"
                       (("PY_LIBS=.*")
                        (string-append "PY_LIBS=\"" python
                                       "/lib/libpython" py-version ".so "
                                       " -lpthread -ldl  -lutil -lm" ; LIB[SMC]
                                       " -lpython" py-version
                                       " -lreadline -lssl -lcrypto"
                                       " -ltk" tcl-version " -ltcl" tcl-version
                                       " -lgdbm -ltirpc -lnsl -lz\"\n"))
                       (("PY_LDFLAGS=.*")
                        (string-append
                          "PY_LDFLAGS=\"-Wl,-rpath=" python "/lib\"\n"))
                       (("PY_INCLUDES=.*")
                        (string-append
                          "PY_INCLUDES=-I" python "/include/python" py-version "\n")))
                     (invoke "autoreconf" "-vfi"))))
               (add-after 'bootstrap-mod-python 'patch-bin-file-mod-python
                 (assoc-ref gnu:%standard-phases 'patch-usr-bin-file))
               (add-after 'patch-bin-file-mod-python 'patch-source-shebangs-mod-python
                 (assoc-ref gnu:%standard-phases 'patch-source-shebangs))
               (add-after 'patch-source-shebangs-mod-python 'configure-mod-python
                 (lambda* args
                   ((assoc-ref gnu:%standard-phases 'configure)
                    #:outputs %outputs
                    #:inputs %build-inputs
                    #:configure-flags (list (string-append "--with-apxs="
                                                           (assoc-ref %outputs "out")
                                                           "/bin/apxs")))))
               (add-after 'configure-mod-python 'patch-more-shebangs-mod-python
                 (assoc-ref gnu:%standard-phases 'patch-generated-file-shebangs))
               (add-after 'patch-more-shebangs-mod-python 'make-mod-python
                 (assoc-ref gnu:%standard-phases 'build))
               (add-after 'make-mod-python 'install-mod-python
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (install-file "src/mod_python.so" (string-append out "/modules"))
                     (with-directory-excursion "dist"
                       (invoke "python" "setup.py" "install" "--root=/"
                               (string-append "--prefix=" out)))
                     #t)))
               (add-after 'install-mod-python 'wrap-programs
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (python (assoc-ref inputs "python"))
                          (py-version (python:python-version python)))
                     ;; httpd needs to be able to find mod_python.
                     ;; mod_python should have at least Python and its
                     ;; C extensions in its PYTHONPATH.
                     (wrap-program (string-append out "/bin/httpd")
                       `("PYTHONPATH" ":" prefix
                         (,(string-append out "/lib/python" py-version "/site-packages")
                          ,(string-append python "/lib/python" py-version)
                          ,(string-append python "/lib/python" py-version "/lib-dynload"))))
                     #t))))))))
    (native-inputs
     `(,@(package-native-inputs httpd-2.2)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("flex" ,flex)
       ("mod-python"
        ,(origin
           (method url-fetch)
           (uri "https://archive.apache.org/dist/httpd/modpython/mod_python-3.3.1.tgz")
           (sha256
            (base32
             "0sss2xi6l1a2z8y6ji0cp8vgyvnhq8zrg0ilkvpj1mygbzyk28xd"))
           (patches
             (list
               ;; Use pkg-config to find Python.
               (origin
                 (method url-fetch)
                 (uri "https://sources.debian.org/data/main/liba/libapache2-mod-python/3.3.1-11/debian/patches/04_autoconf_python_multiarch.patch")
                 (file-name "mod-python-python-discovery.patch")
                 (sha256
                  (base32
                   "0n3zp8j6q0mp0scry7d2hi0baqkim42bqq2c81p4l6mizsy8ry4h")))
               ;; Compatibility with apr@1.3+
               (origin
                 (method url-fetch)
                 (uri "https://sources.debian.org/data/main/liba/libapache2-mod-python/3.3.1-11/debian/patches/10_bts521965.patch")
                 (file-name "mod-python-apr13-compat.patch")
                 (sha256
                  (base32
                   "1k2cd2r13938fbm473sn0ivicaylkcqigyqn2wjir9ppch98kybg")))))))))
    (inputs
     `(,@(package-inputs httpd-2.2)
       ,@(package-inputs python-2.4)
       ("python" ,python-2.4)))))
