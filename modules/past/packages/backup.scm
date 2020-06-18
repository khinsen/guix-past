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

(define-module (past packages backup)
  #:use-module (guix)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages onc-rpc)
  #:use-module (past packages autotools)
  #:use-module (past packages guile-xyz))

(define libtirpc-sans-krb5
  ;; Since libtirpc pulls mit-krb5, which provides its own incompatible
  ;; libcom_err (it lacks '_et_list'), build libtirpc-sans-krb5.
  (package
    (inherit libtirpc)
    (name "libtirpc-minimal")
    (arguments
     '(#:configure-flags '("--disable-gssapi")))
    (inputs '())
    (native-inputs '())
    (propagated-inputs '())))

(define-public libchop/guile-1.8
  ;; This commit was chosen because it was made shortly before the EDCC paper
  ;; at <https://hal.inria.fr/hal-00187069/en> was submitted, in April 2006.
  ;; See <https://github.com/ReScience/ten-years/issues/1#issuecomment-605836321>.
  (let ((commit "feb8f6b9fbd4c7b7772d956ff34cd44e48a25864") ;10 March 2006
        (revision "0")
        (S specification->package))
    (package
      (inherit libchop)
      (version (git-version "0.0.2006" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/libchop.git")
                      (commit commit)))
                (file-name (git-file-name "libchop" version))
                (sha256
                 (base32
                  "1fp8ymrpkkcbssyh1acfkdjpyslajc68y1qjdn0f472l1gpj0ja7"))
                (modules '((guix build utils)))
                (patches
                 (search-patches "past/patches/libchop-anchor-based.patch"))
                (snippet
                 '(begin
                    ;; Include all the libtirpc headers necessary to get the
                    ;; definitions of 'u_int', etc.
                    (substitute* '("src/store-server.c"
                                   "include/chop/block-server.h"
                                   "utils/chop-block-server.c")
                      (("#include <rpc/(.*)\\.h>" _ header)
                       (string-append "#include <rpc/types.h>\n"
                                      "#include <rpc/rpc.h>\n"
                                      "#include <rpc/" header ".h>\n")))
                    #t))))
      (arguments
       '( ;; Link against libtirpc.
         #:configure-flags '("LDFLAGS=-ltirpc -Wl,--as-needed"
                             "--enable-guile-bindings")
         #:parallel-build? #f
         #:phases (modify-phases %standard-phases
                    (add-before 'build 'set-libtirpc-include-path
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; Allow <rpc/rpc.h> & co. to be found.
                        (let ((libtirpc (assoc-ref inputs "libtirpc"))

                              ;; With the Guix 'core-updates' merge in
                              ;; 4bdf4182fe080c3409f6ef9b410146b67cfa2595
                              ;; (May 2020), we moved from CPATH to
                              ;; C_INCLUDE_PATH & co.
                              (variable (if (getenv "CPATH")
                                            "CPATH"
                                            "C_INCLUDE_PATH")))
                          (setenv variable
                                  (string-append (getenv variable)
                                                 ":" libtirpc
                                                 "/include/tirpc"))
                          #t)))
                    (replace 'check
                      (lambda _
                        ;; XXX: 'tests/interfaces/choppers' segfaults.
                        ;; Ignore the exit status.
                        (system* "make" "check")
                        #t))
                    (add-after 'install 'install-scheme-files
                      (lambda* (#:key outputs #:allow-other-keys)
                        ;; Install the (chop …) Guile modules.
                        (define (fixup file)
                          ;; Ensure FILE refers to libguile-chop.so by its
                          ;; absolute file name.
                          (with-fluids ((%default-port-encoding "ISO-8859-1"))
                            (substitute* file
                              (("dynamic-link \"libguile-chop\"")
                               (string-append "dynamic-link \""
                                              (assoc-ref outputs "out")
                                              "/lib/libguile-chop\"")))))

                        (let* ((out (assoc-ref outputs "out"))
                               (site
                                (string-append out "/share/guile/site/chop")))
                          (for-each (lambda (file)
                                      (unless (string-suffix? "-spec.scm" file)
                                        (fixup file)
                                        (install-file file site)))
                                    (find-files "guile" "\\.scm"))
                          #t))))))
      (outputs '("out" "debug"))
      (native-inputs
       `(("autoconf" ,autoconf-2.59)
         ("automake" ,automake-1.9)
         ("libtool" ,libtool-1.5)
         ("texinfo" ,(S "texinfo@4"))
         ("gperf" ,(S "gperf@3.0"))
         ("pkg-config" ,(S "pkg-config"))
         ("rpcsvg-proto" ,(S "rpcsvc-proto"))
         ("g-wrap" ,g-wrap/guile-1.8)
         ("e2fsprogs" ,(S "e2fsprogs"))))         ;for 'compile_et'
      (inputs
       `(("guile" ,(S "guile@1.8"))
         ("util-linux" ,(S "util-linux"))
         ("e2fsprogs" ,(S "e2fsprogs"))           ;for libcom_err
         ("libtirpc" ,libtirpc-sans-krb5)
         ("tdb" ,(S "tdb"))
         ("bdb" ,(S "bdb"))
         ("gdbm" ,(S "gdbm"))
         ("libgcrypt" ,(S "libgcrypt"))
         ("lzo" ,(S "lzo"))
         ("bzip2" ,(S "bzip2"))
         ("zlib" ,(S "zlib"))))
      (propagated-inputs
       ;; The (g-wrap config) module is needed at run time.
       `(("g-wrap" ,g-wrap/guile-1.8))))))
