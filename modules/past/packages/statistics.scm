;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (guix build-system r)
  #:use-module (gnu packages gcc)
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
    (properties '((release-date . "2013-03-01")))))

(define-public r-minimal-2
  (package
    (inherit r-with-tests-2)
    (name "r-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments r-with-tests-2)
       ((#:tests? _ #f) #f)
       ((#:configure-flags flags)
        ;; Do not build the recommended packages.  The build system creates
        ;; random temporary directories and embeds their names in some
        ;; package files.  We build these packages with the r-build-system
        ;; instead.
        `(cons* "--without-recommended-packages"
                "CFLAGS=-O2 -g -fcommon"      ;'-fcommon' needed with GCC 10+
                "FFLAGS=-O2 -g -fallow-argument-mismatch" ;likewise
                ,flags))))))

(define-public r-2-lattice
  (package
    (inherit r-lattice)
    (name "r-lattice")
    (version "0.20-31")
    (source
      (origin
        (method url-fetch)
        (uri (cran-uri "lattice" version))
        (sha256
         (base32
          "1b3m3rg1zd8ssk5jjswk5y93js89vh6939kfajh6i6wphndxigb1"))))
    (build-system r-build-system)
    (arguments
     `(#:r ,r-minimal-2))))

(define-public r-2-matrix
  (package
    (inherit r-matrix)
    (name "r-matrix")
    (version "1.2-0")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "Matrix" version))
       (sha256
        (base32
         "0ywz213p6cpwnklxd81hzdyxjzagaj6cn32ycc5rcnhxy30d7kk5"))))
    (arguments
     `(#:r ,r-minimal-2))
    (propagated-inputs
     `(("r-lattice" ,r-2-lattice)))))

(define-public r-2-survival
  (package
    (inherit r-survival)
    (name "r-survival")
    (version "2.41-3")
    (source
     (origin
       (method url-fetch)
       (uri (cran-uri "survival" version))
       (sha256
        (base32
         "07cnr0hnki6ybbjll54l4s5lllhk19vni5f8m0mvsfp99ls7qygk"))))
    (arguments
     `(#:r ,r-minimal-2))
    (propagated-inputs
     `(("r-matrix" ,r-2-matrix)))))


;; R 3
(define-public r-minimal-3.3.1
  (package (inherit r-minimal)
    (version "3.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-prefix version 1) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1qm9znh8akfy9fkzzi6f1vz2w1dd0chsr6qn7kw80lqzhgjrmi9x"))))
    (arguments
     (substitute-keyword-arguments (package-arguments r-minimal)
       ((#:make-flags flags ''())
        `(cons "CFLAGS=-fcommon" ,flags))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           ;; The test doesn't recognize that zlib 1.2.11 is newer
           ;; than 1.2.5...
           (add-after 'unpack 'bypass-zlib-test
             (lambda _
               (substitute* "configure"
                 (("strncmp\\(ZLIB_VERSION.*")
                  "0);\n"))))))))
    (inputs
     (modify-inputs (package-inputs r-minimal)
       (replace "gfortran" gfortran-7)
       (replace "gcc" gcc-7)))))

(define-public r-minimal-3.3.2
  (package (inherit r-minimal-3.3.1)
    (version "3.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-prefix version 1) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0k2i9qdd83g09fcpls2198q4ykxkii5skczb514gnx7mx4hsv56j"))))))

(define-public r-minimal-3.3.3
  (package (inherit r-minimal-3.3.1)
    (version "3.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-prefix version 1) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0v7wpj89b0i3ad3fi1wak5c93hywmbxv8sdnixhq8l17782nidss"))))))

(define-public r-minimal-3.4.2
  (package (inherit r-minimal-3.3.1)
    (version "3.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-prefix version 1) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0r0cv2kc3x5z9xycpnxx6fbvv22psw2m342jhpslbxkc8g1307lp"))))))

(define-public r-minimal-3.6.3
  (package (inherit r-minimal-3.3.1)
    (version "3.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cran/src/base/R-"
                                  (version-prefix version 1) "/R-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "13xaxwfbzj0bd6rn2n27z0n04lb93mcyq991w4vdbbg8v282jc49"))))))
