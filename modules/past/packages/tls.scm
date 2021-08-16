;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Sergei Trofimovich <slyfox@inbox.ru>
;;; Copyright © 2018 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (past packages tls)
  #:use-module (guix)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages perl))

(define S specification->package)

(define-public openssl-1.0
  ;; This package was marked hidden in Guix in commit
  ;; 12099eac1b161d364be923451d27d7d739d0f14d (August 2021)
  ;; in preparation of removing it entirely.
  (package
    (name "openssl")
    (version "1.0.2u")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://www.openssl.org/source/openssl-"
                                        version ".tar.gz")
                         (string-append "ftp://ftp.openssl.org/source/"
                                        "openssl-" version ".tar.gz")
                         (string-append "ftp://ftp.openssl.org/source/old/"
                                        (string-trim-right version char-set:letter)
                                        "/openssl-" version ".tar.gz")))
              (sha256
               (base32
                "05lxcs4hzyfqd5jn0d9p0fvqna62v2s4pc9qgmq0dpcknkzwdl7c"))
              (patches (search-patches "past/patches/openssl-runpath.patch"
                                       "past/patches/openssl-c-rehash-in.patch"))))
    (outputs '("out"
               "doc"                            ; 2.4MiB of man3 pages
               "static"))                       ; 5.2MiB of .a files
    (build-system gnu-build-system)
    (arguments
     ;; Parallel build is not supported in 1.0.x.
     `(#:parallel-build? #f
       #:parallel-tests? #f
       #:test-target "test"

       ;; Changes to OpenSSL sometimes cause Perl to "sneak in" to the closure,
       ;; so we explicitly disallow it here.
       #:disallowed-references ,(list (canonical-package perl))

       #:phases
       (modify-phases %standard-phases
        ,@(if (%current-target-system)
            '((add-before 'configure 'set-cross-compile
                (lambda* (#:key target outputs #:allow-other-keys)
                  (setenv "CROSS_COMPILE" (string-append target "-"))
                  (setenv "CONFIGURE_TARGET_ARCH"
                          (cond
                            ((string-prefix? "i586" target)
                             "hurd-x86")
                            ((string-prefix? "i686" target)
                             "linux-x86")
                            ((string-prefix? "x86_64" target)
                             "linux-x86_64")
                            ((string-prefix? "mips64el" target)
                             "linux-mips64")
                            ((string-prefix? "arm" target)
                             "linux-armv4")
                            ((string-prefix? "aarch64" target)
                             "linux-aarch64")
                            ((string-prefix? "powerpc64le" target)
                             "linux-ppc64le")
                            ((string-prefix? "powerpc64" target)
                             "linux-ppc64")
                            ((string-prefix? "powerpc" target)
                             "linux-ppc")
                            ((string-prefix? "riscv64" target)
                             "linux-generic64")))
                  #t)))
            '())
         (add-before 'patch-source-shebangs 'patch-tests
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (let ((bash (assoc-ref (or native-inputs inputs) "bash")))
               (substitute* (find-files "test")
                 (("/bin/sh")
                  (string-append bash "/bin/sh"))
                 (("/bin/rm")
                  "rm"))
               #t)))
         (add-before 'configure 'patch-Makefile.org
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The default MANDIR is some unusual place.  Fix that.
             (let ((out (assoc-ref outputs "out")))
               (patch-makefile-SHELL "Makefile.org")
               (substitute* "Makefile.org"
                 (("^MANDIR[[:blank:]]*=.*$")
                  (string-append "MANDIR = " out "/share/man\n")))
               #t)))
         (replace 'configure
           ;; Override this phase because OpenSSL 1.0 does not understand -rpath.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke ,@(if (%current-target-system)
                             '("./Configure")
                             '("./config"))
                       "shared"                 ; build shared libraries
                       "--libdir=lib"

                       ;; The default for this catch-all directory is
                       ;; PREFIX/ssl.  Change that to something more
                       ;; conventional.
                       (string-append "--openssldir=" out
                                      "/share/openssl-" ,version)

                       (string-append "--prefix=" out)
                       ,@(if (%current-target-system)
                             '((getenv "CONFIGURE_TARGET_ARCH"))
                             '())))))
         (add-after 'install 'move-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move static libraries to the "static" output.
             (let* ((out    (assoc-ref outputs "out"))
                    (lib    (string-append out "/lib"))
                    (static (assoc-ref outputs "static"))
                    (slib   (string-append static "/lib")))
               (for-each (lambda (file)
                           (install-file file slib)
                           (delete-file file))
                         (find-files lib "\\.a$"))
               #t)))
         (add-after 'install 'move-man3-pages
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move section 3 man pages to "doc".
             (let* ((out    (assoc-ref outputs "out"))
                    (man3   (string-append out "/share/man/man3"))
                    (doc    (assoc-ref outputs "doc"))
                    (target (string-append doc "/share/man/man3")))
               (mkdir-p target)
               (for-each (lambda (file)
                           (rename-file file
                                        (string-append target "/"
                                                       (basename file))))
                         (find-files man3))
               (delete-file-recursively man3)
               #t)))
         (add-after 'install 'remove-miscellany
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The 'misc' directory contains random undocumented shell and Perl
             ;; scripts.  Remove them to avoid retaining a reference on Perl.
             (let ((out (assoc-ref outputs "out")))
               (delete-file-recursively (string-append out "/share/openssl-"
                                                       ,version "/misc"))
               #t))))))
    (native-inputs `(("perl" ,(S "perl"))))
    (native-search-paths
     (list (search-path-specification
            (variable "SSL_CERT_DIR")
            (separator #f)                      ; single entry
            (files '("etc/ssl/certs")))
           (search-path-specification
            (variable "SSL_CERT_FILE")
            (file-type 'regular)
            (separator #f)                      ; single entry
            (files '("etc/ssl/certs/ca-certificates.crt")))))
   (properties '((release-date . "2019-12-20")))
   (synopsis "OpenSSL 1.0.2u, released 2019-12-20")
   (description
    "OpenSSL is an implementation of SSL/TLS.  This version is the final
release of the 1.0.2 branch.")
   (home-page "https://www.openssl.org/")
   (license license:openssl)))
