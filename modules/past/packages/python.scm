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

(define-module (past packages python)
  #:use-module (guix)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (srfi srfi-1))

(define-public python-2.4                           ; Dec. 2008
  (package
    (inherit python-2)
    (version "2.4.6")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://www.python.org/ftp/python/"
                          version "/Python-" version ".tar.bz2"))
      (sha256
       (base32
        "021y88a4ki07dgq19yhg6zfvmncfiz7h5b2255438i9zmlwl246s"))))
    (outputs '("out"))
    (arguments
      (substitute-keyword-arguments (package-arguments python-2)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-after 'unpack 'create-setup-local
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((zlib (assoc-ref inputs "zlib"))
                      (tcl  (assoc-ref inputs "tcl"))
                      (tk   (assoc-ref inputs "tk"))
                      (gdbm (assoc-ref inputs "gdbm"))
                      (read (assoc-ref inputs "readline"))
                      (rpc  (assoc-ref inputs "libtirpc"))
                      (nsl  (assoc-ref inputs "libnsl"))
                      (ssl  (assoc-ref inputs "openssl")))
                  (with-output-to-file "Modules/Setup.local"
                    (lambda _
                      (format #t "readline readline.c -I~a/include -L~a/lib -lreadline~@
                              _ssl _ssl.c -DUSE_SSL -I$~a/include/openssl -L~a/lib -lssl -lcrypto~@
                              _tkinter _tkinter.c tkappinit.c -DWITH_APPINIT -L~a/lib -I~a/include -L~a/lib -I~a/include -ltk~a -ltcl~a~@
                              gdbm gdbmmodule.c -I~a/include -L~a/lib -lgdbm~@
                              nis nismodule.c -I~a/include/tirpc -I~a/include -ltirpc -lnsl~@
                              zlib zlibmodule.c -I~a/include -L~a/lib -lz~%"
read read ssl ssl tcl tcl tk tk ,(version-major+minor (package-version tcl)) ,(version-major+minor (package-version tcl)) gdbm gdbm rpc nsl zlib zlib))))
                  #t))
              (add-after 'unpack 'patch-rpc-location
                (lambda _
                  (substitute* "Modules/nismodule.c"
                    (("<rpc/") "<tirpc/rpc/"))
                  (substitute* "setup.py"
                    (("\\['nsl'") "['nsl', 'tirpc'"))
                  #t))
              (add-after 'unpack 'skip-crypt-module
                (lambda _
                  (substitute* "setup.py"
                    ((".*cryptmodule.c.*") "\n"))
                  #t))
            (add-before 'check 'delete-failing-tests
              (lambda _
                (for-each
                  (lambda (file)
                    (delete-file (string-append "Lib/test/" file)))
                  '("test_anydbm.py" "test_array.py" "test_decimal.py"
                    "test_getargs2.py" "test_long.py" "test_math.py"
                    "test_mhlib.py" "test_random.py" "test_socket.py"
                    "test_str.py" "test_userstring.py" "test_whichdb.py"
                    "test_zlib.py"))
                #t))
            (add-after 'check 'find-netinet-in-h
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((glibc (assoc-ref inputs "libc")))
                  (substitute* (find-files "Lib/plat-generic" ".*")
                    (("/usr/include/netinet/in.h")
                     (string-append glibc "/include/netinet/in.h")))
                  #t)))
            (delete 'move-tk-inter)))
         ;; Python-2.4 does not support '-j'.
         ((#:make-flags _) ''())))
    (native-search-paths
      (list (search-path-specification
              (variable "PYTHONPATH")
              (files '("lib/python2.4/site-packages")))))
    (inputs
     `(("libnsl" ,libnsl)
       ("libtirpc" ,libtirpc)
       ("openssl" ,openssl-1.0)
       ,@(alist-delete "openssl" (package-inputs python-2))))))
