;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (past packages perl)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages))

(define-public perl-5.14
  (package
    (name "perl")
    (version "5.14.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/src/5.0/perl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1js47zzna3v38fjnirf2vq6y0rjp8m86ysj5vagzgkig956d8gw0"))
              (patches (search-patches
                        "past/patches/perl-5.14-no-sys-dirs.patch"
                        "past/patches/perl-5.14-autosplit-default-time.patch"
                        "past/patches/perl-5.14-module-pluggable-search.patch"))))
    (properties `((release-date . "2013-03-10")))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out"))
                   (libc (assoc-ref inputs "libc")))
               ;; Use the right path for `pwd'.
               (substitute* "dist/Cwd/Cwd.pm"
                 (("/bin/pwd")
                  (which "pwd")))

               (invoke "./Configure"
                       (string-append "-Dprefix=" out)
                       (string-append "-Dman1dir=" out "/share/man/man1")
                       (string-append "-Dman3dir=" out "/share/man/man3")
                       "-de" "-Dcc=gcc"
                       "-Uinstallusrbinperl"
                       "-Dinstallstyle=lib/perl5"
                       "-Duseshrplib"
                       (string-append "-Dlocincpth=" libc "/include")
                       (string-append "-Dloclibpth=" libc "/lib")

                       ;; Force the library search path to contain only libc
                       ;; because it is recorded in Config.pm and
                       ;; Config_heavy.pl; we don't want to keep a reference
                       ;; to everything that's in $LIBRARY_PATH at build
                       ;; time (Binutils, bzip2, file, etc.)
                       (string-append "-Dlibpth=" libc "/lib")
                       (string-append "-Dplibpth=" libc "/lib")))))

         (add-before 'strip 'make-shared-objects-writable
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The 'lib/perl5' directory contains ~50 MiB of .so.  Make them
             ;; writable so that 'strip' actually strips them.
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (for-each (lambda (dso)
                           (chmod dso #o755))
                         (find-files lib "\\.so$"))
               #t))))))
    (native-search-paths (list (search-path-specification
                                (variable "PERL5LIB")
                                (files '("lib/perl5/site_perl")))))
    (home-page "https://www.perl.org/")
    (synopsis "Implementation of the Perl programming language")
    (description
     "Perl is a general-purpose programming language originally developed for
text manipulation and now used for a wide range of tasks including system
administration, web development, network programming, GUI development, and
more.")
    (license license:gpl1+)))

(define-public perl-5.24
  (package
    (name "perl")
    (version "5.24.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/src/5.0/perl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "00jj8zr8fnihrxxhl8h936ssczv5x86qb618yz1ig40d1rp0qhvy"))
              (patches (search-patches
                        "past/patches/perl-5.24-no-sys-dirs.patch"
                        "past/patches/perl-5.24-autosplit-default-time.patch"
                        "past/patches/perl-5.24-deterministic-ordering.patch"
                        "past/patches/perl-5.24-reproducible-build-date.patch"))))
    (properties `((release-date . "2018-04-14")))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:configure-flags
       (let ((out  (assoc-ref %outputs "out"))
             (libc (assoc-ref %build-inputs "libc")))
         (list
          (string-append "-Dprefix=" out)
          (string-append "-Dman1dir=" out "/share/man/man1")
          (string-append "-Dman3dir=" out "/share/man/man3")
          "-de" "-Dcc=gcc"
          "-Uinstallusrbinperl"
          "-Dinstallstyle=lib/perl5"
          "-Duseshrplib"
          (string-append "-Dlocincpth=" libc "/include")
          (string-append "-Dloclibpth=" libc "/lib")
          "-Dusethreads"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'setup-configure
           (lambda _
             ;; Use the right path for `pwd'.
             (substitute* "dist/PathTools/Cwd.pm"
               (("/bin/pwd")
                (which "pwd")))

             ;; Build in GNU89 mode to tolerate C++-style comment in libc's
             ;; <bits/string3.h>.
             (substitute* "cflags.SH"
               (("-std=c89")
                "-std=gnu89"))))
         (replace 'configure
           (lambda* (#:key configure-flags #:allow-other-keys)
             (format #t "Perl configure flags: ~s~%" configure-flags)
             (apply invoke "./Configure" configure-flags)))
         (add-before 'strip 'make-shared-objects-writable
           (lambda* (#:key outputs #:allow-other-keys)
             ;; The 'lib/perl5' directory contains ~50 MiB of .so.  Make them
             ;; writable so that 'strip' actually strips them.
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (for-each (lambda (dso)
                           (chmod dso #o755))
                         (find-files lib "\\.so$")))))
         (add-after 'install 'remove-extra-references
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (libc    (assoc-ref inputs "libc"))
                    (config1 (car (find-files (string-append out "/lib/perl5")
                                              "^Config_heavy\\.pl$")))
                    (config2 (find-files (string-append out "/lib/perl5")
                                         "^Config\\.pm$")))
               ;; Force the library search path to contain only libc because
               ;; it is recorded in Config.pm and Config_heavy.pl; we don't
               ;; want to keep a reference to everything that's in
               ;; $LIBRARY_PATH at build time (GCC, Binutils, bzip2, file,
               ;; etc.)
               (substitute* config1
                 (("^incpth=.*$")
                  (string-append "incpth='" libc "/include'\n"))
                 (("^(libpth|plibpth|libspath)=.*$" _ variable)
                  (string-append variable "='" libc "/lib'\n")))

               (for-each (lambda (file)
                           (substitute* config2
                             (("libpth => .*$")
                              (string-append "libpth => '" libc
                                             "/lib',\n"))))
                         config2)))))))
    (native-search-paths (list (search-path-specification
                                (variable "PERL5LIB")
                                (files '("lib/perl5/site_perl")))))
    (home-page "https://www.perl.org/")
    (synopsis "Implementation of the Perl programming language")
    (description
     "Perl is a general-purpose programming language originally developed for
text manipulation and now used for a wide range of tasks including system
administration, web development, network programming, GUI development, and
more.")
    (license license:gpl1+)))
