;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2018 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2015, 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
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

(define-module (past packages qt)
  #:use-module (guix)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages python))

(define S specification->package)

(define-public qt-4
  ;; This package was removed from Guix in commit
  ;; a801c7379a534a2896a03a1a6f8b47eb92691b00 (March 2021).
  (package
    (name "qt")
    (version "4.8.7")

    ;; See <https://www.qt.io/blog/2015/05/26/qt-4-8-7-released>.
    (properties '((release-date . "2015-05-26")))

    (source (origin
             (method url-fetch)
             (uri (list (string-append "http://download.qt.io/archive/qt/"
                                       (version-major+minor version)
                                       "/" version
                                       "/qt-everywhere-opensource-src-"
                                       version ".tar.gz")
                        (string-append "http://sources.buildroot.net/qt/"
                                       "qt-everywhere-opensource-src-"
                                       version ".tar.gz")))
             (sha256
              (base32
               "183fca7n7439nlhxyg1z7aky0izgbyll3iwakw4gwivy16aj5272"))
             (patches (search-patches "past/patches/qt4-ldflags.patch"))
             (modules '((guix build utils)))
             (snippet
              ;; Remove webkit module, which is not built.
              '(begin (delete-file-recursively "src/3rdparty/webkit")
                      #t))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("mesa" ,(S "mesa"))))
    (inputs
     `(("alsa-lib" ,(S "alsa-lib"))
       ("bluez" ,(S "bluez"))
       ("cups" ,(S "cups"))
       ("dbus" ,(S "dbus"))
       ("double-conversion" ,(S "double-conversion"))
       ("expat" ,(S "expat"))
       ("fontconfig" ,(S "fontconfig"))
       ("freetype" ,(S "freetype"))
       ("glib" ,(S "glib"))
       ("gstreamer" ,(S "gstreamer"))
       ("gst-plugins-base" ,(S "gst-plugins-base"))
       ("icu4c" ,(S "icu4c"))
       ("jasper" ,(S "jasper"))
       ("libinput" ,(S "libinput-minimal"))
       ("libmng" ,(S "libmng"))
       ("libpci" ,(S "pciutils"))
       ("libpng" ,(S "libpng"))
       ("libtiff" ,(S "libtiff"))
       ("libwebp" ,(S "libwebp"))
       ("libx11" ,(S "libx11"))
       ("libxcomposite" ,(S "libxcomposite"))
       ("libxcursor" ,(S "libxcursor"))
       ("libxext" ,(S "libxext"))
       ("libxfixes" ,(S "libxfixes"))
       ("libxi" ,(S "libxi"))
       ("libxinerama" ,(S "libxinerama"))
       ("libxkbcommon" ,(S "libxkbcommon"))
       ("libxml2" ,(S "libxml2"))
       ("libxrandr" ,(S "libxrandr"))
       ("libxrender" ,(S "libxrender"))
       ("libxslt" ,(S "libxslt"))
       ("libxtst" ,(S "libxtst"))
       ("mtdev" ,(S "mtdev"))
       ("mariadb-dev" ,(S "mariadb") "dev")
       ("nss" ,(S "nss"))
       ("postgresql" ,(S "postgresql"))
       ("pulseaudio" ,(S "pulseaudio"))
       ("pcre2" ,(S "pcre2"))
       ("sqlite" ,(S "sqlite"))
       ("udev" ,(S "eudev"))
       ("unixodbc" ,(S "unixodbc"))
       ("wayland" ,(S "wayland"))
       ("xcb-util" ,(S "xcb-util"))
       ("xcb-util-image" ,(S "xcb-util-image"))
       ("xcb-util-keysyms" ,(S "xcb-util-keysyms"))
       ("xcb-util-renderutil" ,(S "xcb-util-renderutil"))
       ("xcb-util-wm" ,(S "xcb-util-wm"))
       ("zlib" ,(S "zlib"))
       ("libjpeg" ,(S "libjpeg-turbo"))
       ("libsm" ,(S "libsm"))
       ("openssl" ,(S "openssl@1.0"))))
    (native-inputs
     `(;; XXX: The JavaScriptCore engine does not build with the C++11 standard.
       ;; We could build it with -std=gnu++98, but then we'll get in trouble with
       ;; ICU later.  Just keep using GCC 5 for now.
       ("gcc@5" ,gcc-5)
       ("bison" ,(S "bison"))
       ("flex" ,(S "flex"))
       ("gperf" ,(S "gperf"))
       ("perl" ,(S "perl"))
       ("pkg-config" ,(S "pkg-config"))
       ("python" ,python-2)
       ("ruby" ,(S "ruby"))
       ("which" ,(S "which"))))
    ;; Note: there are 37 MiB of examples and a '-exampledir' configure flags,
    ;; but we can't make them a separate output because "out" and "examples"
    ;; would refer to each other.
    (outputs '("out"                             ;112MiB core + 37MiB examples
               "doc"))                           ;280MiB of HTML + code
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'set-paths 'hide-default-gcc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gcc (assoc-ref inputs "gcc")))
               ;; Remove the default GCC from CPLUS_INCLUDE_PATH to prevent
               ;; conflicts with the GCC 5 input.
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gcc "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                        ":"))
               #t)))
         (replace
          'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (doc (assoc-ref outputs "doc")))
              (substitute* '("configure")
                (("/bin/pwd") (which "pwd")))
              (substitute* "src/corelib/global/global.pri"
                (("/bin/ls") (which "ls")))

              (invoke
                "./configure"
                "-verbose"
                "-prefix" out
                "-nomake" "examples demos"
                ;; Note: Don't pass '-docdir' since 'qmake' and
                ;; libQtCore would record its value, thereby defeating
                ;; the whole point of having a separate output.
                "-datadir" (string-append out "/share/qt-" ,version
                                          "/data")
                "-importdir" (string-append out "/lib/qt-4"
                                            "/imports")
                "-plugindir" (string-append out "/lib/qt-4"
                                            "/plugins")
                "-translationdir" (string-append out "/share/qt-" ,version
                                                 "/translations")
                "-demosdir"    (string-append out "/share/qt-" ,version
                                              "/demos")
                "-examplesdir" (string-append out "/share/qt-" ,version
                                              "/examples")
                "-opensource"
                "-confirm-license"
                ;; explicitly link with dbus instead of dlopening it
                "-dbus-linked"
                ;; Skip the webkit module; it fails to build on armhf
                ;; and, apart from that, may pose security risks.
                "-no-webkit"
                ;; don't use the precompiled headers
                "-no-pch"
                ;; drop special machine instructions not supported
                ;; on all instances of the target
                ,@(if (string-prefix? "x86_64"
                                      (or (%current-target-system)
                                          (%current-system)))
                      '()
                      '("-no-mmx"
                        "-no-3dnow"
                        "-no-sse"
                        "-no-sse2"))
                "-no-sse3"
                "-no-ssse3"
                "-no-sse4.1"
                "-no-sse4.2"
                "-no-avx"))))
         (add-after
          'install 'move-doc
          (lambda* (#:key outputs #:allow-other-keys)
            ;; Because of qt4-documentation-path.patch, documentation ends up
            ;; being installed in OUT.  Move it to the right place.
            (let* ((out    (assoc-ref outputs "out"))
                   (doc    (assoc-ref outputs "doc"))
                   (olddoc (string-append out "/doc"))
                   (docdir (string-append doc "/share/doc/qt-" ,version)))
              (mkdir-p (dirname docdir))

              ;; Note: We can't use 'rename-file' here because OUT and DOC are
              ;; different "devices" due to bind-mounts.
              (copy-recursively olddoc docdir)
              (delete-file-recursively olddoc)
              #t))))))
    (native-search-paths
     (list (search-path-specification
            (variable "QMAKEPATH")
            (files '("lib/qt5")))
           (search-path-specification
            (variable "QML2_IMPORT_PATH")
            (files '("lib/qt5/qml")))
           (search-path-specification
            (variable "QT_PLUGIN_PATH")
            (files '("lib/qt5/plugins")))
           (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))
           (search-path-specification
            (variable "XDG_CONFIG_DIRS")
            (files '("etc/xdg")))))
    (home-page "https://www.qt.io/")
    (synopsis "Cross-platform GUI library")
    (description "Qt is a cross-platform application and UI framework for
developers using C++ or QML, a CSS & JavaScript like language.")
    (license (list license:lgpl2.1 license:lgpl3))

    ;; Qt 4: 'QBasicAtomicPointer' leads to build failures on MIPS;
    ;; see <http://hydra.gnu.org/build/112828>.
    ;; Qt 5: assembler error; see <http://hydra.gnu.org/build/112526>.
    (supported-systems (delete "mips64el-linux" %supported-systems))))
