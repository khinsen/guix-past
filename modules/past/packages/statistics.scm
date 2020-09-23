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

(define-module (past packages statistics)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages texinfo)
  #:use-module (srfi srfi-1))

;; r-with-tests is private so we inherit from r-minimal.
(define r-with-tests-2
  (package
    (inherit r-minimal)
    (name "r-with-tests")
    (version "2.15.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cran/src/base"
                            "/R-" (version-major version)
                            "/R-" version ".tar.gz"))
        (sha256
         (base32
          "1mjmq95s5nrwppbzic6lzanjq65j3sxg85l1q09c0fxdin7s70y5"))
        (modules '((guix build utils)))
        (snippet
         '(begin (delete-file "NEWS.pdf") #t))))
    (arguments
     (substitute-keyword-arguments (package-arguments r-minimal)
       ((#:tests? _ #t) #t)
       ((#:configure-flags cf)
        `(cons* "--with-system-zlib"
                "--with-system-bzlib"
                "--with-system-pcre"
                (delete "--without-recommended-packages" ,cf)))
       ((#:phases phases)
        `(modify-phases ,phases
           ;; We can only use some of 'build-reproducibly with this older version.
           (replace 'build-reproducibly
             (lambda _
               ;; The documentation contains time stamps to demonstrate
               ;; documentation generation in different phases.
               (substitute* "src/library/tools/man/Rd2HTML.Rd"
                 (("\\\\%Y-\\\\%m-\\\\%d at \\\\%H:\\\\%M:\\\\%S")
                  "(removed for reproducibility)"))

               ;; Remove timestamp from tracing environment.  This fixes
               ;; reproducibility of "methods.rd{b,x}".
               (substitute* "src/library/methods/R/trace.R"
                 (("dateCreated = Sys.time\\(\\)")
                  "dateCreated = as.POSIXct(\"1970-1-1 00:00:00\", tz = \"UTC\")"))

               ;; Ensure that gzipped files are reproducible.
               (substitute* '("src/library/grDevices/Makefile.in"
                              "doc/manual/Makefile.in")
                 (("R_GZIPCMD\\)" line)
                  (string-append line " -n")))

               ;; The "srcfile" procedure in "src/library/base/R/srcfile.R"
               ;; queries the mtime of a given file and records it in an object.
               ;; This is acceptable at runtime to detect stale source files,
               ;; but it destroys reproducibility at build time.

               ;; Similarly, the "srcfilecopy" procedure records the current
               ;; time.  We change both of them to respect SOURCE_DATE_EPOCH.
;               (substitute* "src/library/base/R/srcfile.R"
;                 (("timestamp <- (timestamp.*|file.mtime.*)" _ time)
;                  (string-append "timestamp <- \
;as.POSIXct(if (\"\" != Sys.getenv(\"SOURCE_DATE_EPOCH\")) {\
;  as.numeric(Sys.getenv(\"SOURCE_DATE_EPOCH\"))\
;} else { " time "}, origin=\"1970-01-01\")\n")))

               ;; This library is installed using "install_package_description",
               ;; so we need to pass the "builtStamp" argument.
               ;(substitute* "src/library/tools/Makefile.in"
               ;  (("(install_package_description\\(.*\"')\\)\"" line prefix)
               ;   (string-append prefix ", builtStamp='1970-01-01')\"")))

               (substitute* "src/library/Recommended/Makefile.in"
                 (("INSTALL_OPTS =" m)
                  (string-append m " --built-timestamp=1970-01-01" m)))

               ;; R bundles an older version of help2man, which does not respect
               ;; SOURCE_DATE_EPOCH.  We cannot just use the latest help2man,
               ;; because that breaks a test.
               (with-fluids ((%default-port-encoding "ISO-8859-1"))
                 (substitute* "tools/help2man.pl"
                   (("my \\$date = strftime \"%B %Y\", localtime" line)
                    (string-append line " 1"))))
               #t))))))
    (native-inputs
     `(("texinfo" ,texinfo-4)
       ,@(alist-delete "texinfo" (package-native-inputs r-minimal))))
    (properties '((release-date "2013-03-01")))))
