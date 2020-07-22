;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (past packages boost)
  #:use-module (guix)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc))

(define S specification->package)

(define-public boost-1.58
  (package
    (name "boost")
    (version "1.58.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost/" version "/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "1rfkqxns60171q62cppiyzj8pmsbwp1l8jd7p6crriryqd7j1z7x"))))
    (properties `((release-date . "2015-04-17")))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; TODO
       #:make-flags
       (list "threading=multi" "link=shared"

             ;; Set the RUNPATH to $libdir so that the libs find each other.
             (string-append "linkflags=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib")

             ;; Boost's 'context' library is not yet supported on mips64, so
             ;; we disable it.  The 'coroutine' library depends on 'context',
             ;; so we disable that too.
             ,@(if (string-prefix? "mips64" (or (%current-target-system)
                                                (%current-system)))
                   '("--without-context"
                     "--without-coroutine" "--without-coroutine2")
                   '()))
       #:phases
       (modify-phases %standard-phases
         (delete 'bootstrap)
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* '("libs/config/configure"
                              "libs/spirit/classic/phoenix/test/runtest.sh"
                              "tools/build/doc/bjam.qbk"
                              "tools/build/src/engine/execunix.c"
                              "tools/build/src/engine/Jambase"
                              "tools/build/src/engine/jambase.c")
                 (("/bin/sh") (which "sh")))

               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))

               (invoke "./bootstrap.sh"
                       (string-append "--prefix=" out)
                       "--with-toolset=gcc"))))
         (replace 'build
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (apply invoke "./b2"
                    (format #f "-j~a" (parallel-job-count))
                    make-flags)))
         (replace 'install
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (apply invoke "./b2" "install" make-flags))))))
    (native-inputs
     `(("perl" ,(S "perl"))
       ("python" ,(S "python@2"))
       ("tcsh" ,(S "tcsh"))))
    (inputs `(("zlib" ,(S "zlib"))))
    (home-page "https://boost.org")
    (synopsis "Peer-reviewed portable C++ source libraries")
    (description
     "A collection of libraries intended to be widely useful, and
usable across a broad spectrum of applications.")
    (license (license:x11-style "http://www.boost.org/LICENSE_1_0.txt"
                                "Some components have other similar licences."))))

(define-public boost-1.55
  (package
    (inherit boost-1.58)
    (name "boost")
    (version "1.55.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost/" version "/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "0lkv5dzssbl5fmh2nkaszi8x9qbj80pr4acf9i26sj3rvlih1w7z"))))
    (properties `((release-date . "2013-11-11")))
    (arguments
     `(#:tests? #f ; TODO
       #:make-flags
       (list "threading=multi" "link=shared"

             ;; Set the RUNPATH to $libdir so that the libs find each other.
             (string-append "linkflags=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib")

             ;; Boost's 'context' library is not yet supported on mips64, so
             ;; we disable it.  The 'coroutine' library depends on 'context',
             ;; so we disable that too.
             ,@(if (string-prefix? "mips64" (or (%current-target-system)
                                                (%current-system)))
                   '("--without-context"
                     "--without-coroutine" "--without-coroutine2")
                   '()))
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'hide-default-gcc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               ;; Remove the default GCC from CPLUS_INCLUDE_PATH to prevent
               ;; conflicts with the custom GCC input.
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gcc "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                        ":"))
               #t)))
         (delete 'bootstrap)
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* '("libs/config/configure"
                              "libs/spirit/classic/phoenix/test/runtest.sh"
                              "tools/build/v2/doc/bjam.qbk"
                              "tools/build/v2/engine/execunix.c"
                              "tools/build/v2/engine/Jambase"
                              "tools/build/v2/engine/jambase.c")
                 (("/bin/sh") (which "sh")))

               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))

               (invoke "./bootstrap.sh"
                       (string-append "--prefix=" out)
                       "--with-toolset=gcc"))))
         (replace 'build
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (apply invoke "./b2"
                    (format #f "-j~a" (parallel-job-count))
                    make-flags)))
         (replace 'install
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (apply invoke "./b2" "install" make-flags))))))
    (native-inputs
     `(("gcc@4" ,gcc-4.9)))))

(define-public boost-1.44
  (package
    (inherit boost-1.55)
    (name "boost")
    (version "1.44.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/boost/boost/" version "/boost_"
                    (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                    ".tar.bz2"))
              (sha256
               (base32
                "1nvq36mvzr1fr85q0jh86rk3bk65s1y55jgqgzfg3lcpkl12ihs5"))))
    (properties `((release-date . "2010-08-13")))
    (arguments
     `(#:tests? #f
       #:make-flags
       (list "threading=multi" "link=shared"
	         (string-append "-sICU_PATH=" (assoc-ref %build-inputs "icu4c"))
             "-sHAVE_ICU=1"
             ;; Set the RUNPATH to $libdir so that the libs find each other.
             (string-append "linkflags=-Wl,-rpath="
                            (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'hide-default-gcc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               ;; Remove the default GCC from CPLUS_INCLUDE_PATH to prevent
               ;; conflicts with the custom GCC input.
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gcc "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                        ":"))
               #t)))
         (delete 'bootstrap)
         ;; See https://svn.boost.org/trac/boost/ticket/6165
         (add-after 'unpack 'fix-threads-detection
           (lambda _
             (substitute* "boost/config/stdlib/libstdcpp3.hpp"
               (("_GLIBCXX_HAVE_GTHR_DEFAULT")
                "_GLIBCXX_HAS_GTHREADS"))
             #t))
         ;; See https://svn.boost.org/trac/boost/ticket/6940
         (add-after 'unpack 'fix-TIME_UTC
           (lambda _
             (substitute* '("libs/interprocess/test/util.hpp"
                            "libs/interprocess/test/condition_test_template.hpp"
                            "libs/spirit/classic/test/grammar_mt_tests.cpp"
                            "libs/spirit/classic/test/owi_mt_tests.cpp"
                            "libs/thread/src/pthread/thread.cpp"
                            "libs/thread/src/pthread/timeconv.inl"
                            "libs/thread/src/win32/timeconv.inl"
                            "libs/thread/test/util.inl"
                            "libs/thread/test/test_xtime.cpp"
                            "libs/thread/example/xtime.cpp"
                            "libs/thread/example/tennis.cpp"
                            "libs/thread/example/starvephil.cpp"
                            "libs/thread/example/thread.cpp"
                            "boost/thread/xtime.hpp")
               (("TIME_UTC") "TIME_UTC_"))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* '("libs/config/configure"
                              "libs/spirit/classic/phoenix/test/runtest.sh"
                              "tools/jam/doc/bjam.qbk"
                              "tools/jam/src/execunix.c"
                              "tools/jam/src/Jambase"
                              "tools/jam/src/jambase.c")
                 (("/bin/sh") (which "sh")))

               (setenv "SHELL" (which "sh"))
               (setenv "CONFIG_SHELL" (which "sh"))

               (invoke "./bootstrap.sh"
                       (string-append "--prefix=" out)
                       "--with-toolset=gcc"))))
         (replace 'build
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             (apply invoke "./bjam"
                    (format #f "-j~a" (parallel-job-count))
                    make-flags)))
         (replace 'install
           (lambda* (#:key outputs make-flags #:allow-other-keys)
	         (apply invoke "./bjam" "install" make-flags))))))
    (inputs
     `(("icu4c" ,(S "icu4c"))
       ,@(package-inputs boost-1.55)))
    (native-inputs
     `(("gcc@4" ,gcc-4.9)
       ,@(package-native-inputs boost-1.55)))))
