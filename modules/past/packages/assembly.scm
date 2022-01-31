;;; Guix Past --- Packages from the past for GNU Guix.
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
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

(define-module (past packages assembly)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages assembly))

(define (rgbds-with-version version source-hash)
  (package
    (inherit rgbds)
    (name "rgbds")
    (version version)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rednex/rgbds.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256 (base32 source-hash))))))

;; The tests are failing for the 0.2 releases, the ones before 0.2.3 don't even
;; provide them
(define (without-tests base-rgbds)
  (package
    (inherit base-rgbds)
    (arguments
      (substitute-keyword-arguments (package-arguments base-rgbds)
        ((#:phases phases)
         `(modify-phases ,phases
            (delete 'check)))))))

;; RGBDS versions before 0.1.2 fail to build.

(define-public rgbds-0.1.2
  (let ((base-rgbds
          (rgbds-with-version
            "0.1.2" "1haqswx50hsgfanz9j17y437ciwvbq8lpw445zfiqipawh45a415")))
    (package
      (inherit (without-tests base-rgbds))
      (license (license:non-copyleft "file://LICENSE")))))

(define-public rgbds-0.2.0
  (let ((base-rgbds
          (rgbds-with-version
            "0.2.0" "1q9jci95jrvgc2cyzpsskx92l4m3sv3jyy59bmld3qhqh3nw6jwd")))
    (package
      (inherit (without-tests base-rgbds))
      (license (license:non-copyleft "file://LICENSE")))))

(define-public rgbds-0.2.1
  (let ((base-rgbds
          (rgbds-with-version
            "0.2.1" "06hxdq4b9y4bd8c89x4baia18s34814jikgj86d0hjxkqx4ki204")))
    (package
      (inherit (without-tests base-rgbds))
      (license (license:non-copyleft "file://LICENSE")))))

(define-public rgbds-0.2.2
  (let ((base-rgbds
          (rgbds-with-version
            "0.2.2" "0ga6myr737wxvbldm886chxca2d6i5jnbzqac0xakf87il1i6kb3")))
    (package
      (inherit (without-tests base-rgbds))
      (license (license:non-copyleft "file://LICENSE")))))

(define-public rgbds-0.2.3
  (let ((base-rgbds
          (rgbds-with-version
            "0.2.3" "05fkqrn0fiins61aq3iwzmkm0ii5ihqskv2xv0wk8xd8fp4j2ga1")))
    (package
      (inherit (without-tests base-rgbds))
      (license (license:non-copyleft "file://LICENSE")))))

(define-public rgbds-0.2.4
  (let ((base-rgbds
          (rgbds-with-version
            "0.2.4" "0dwq0p9g1lci8sm12a2rfk0g33z2vr75x78zdf1g84djwbz8ipc6")))
    (package
      (inherit (without-tests base-rgbds))
      (license (license:non-copyleft "file://LICENSE")))))

(define-public rgbds-0.2.5
  (let ((base-rgbds
          (rgbds-with-version
            "0.2.5" "11lri6p1pr3byxrmbz542263587smb4czq46vr1kyn94lxa3ikbp")))
    (package
      (inherit (without-tests base-rgbds))
      (license (license:non-copyleft "file://LICENSE")))))

(define-public rgbds-0.3.0
  (let ((base-rgbds
          (rgbds-with-version
            "0.3.0" "19gk9lakrpn46waxvd4v8fa86xig006r1xvhgqw2acdc50xnxqmk")))
    (package
      (inherit base-rgbds)
      (license (license:non-copyleft "file://LICENSE.md")))))

(define-public rgbds-0.3.1
  (let ((base-rgbds
          (rgbds-with-version
            "0.3.1" "1bx5yhdyp22q3k9x2kly54pmq78aikx8pmygbhz34mgv8n5w4qss")))
    (package
      (inherit base-rgbds)
      (license (license:non-copyleft "file://LICENSE.md")))))

(define-public rgbds-0.3.2
  (let ((base-rgbds
          (rgbds-with-version
            "0.3.2" "034l1xqp46h7yjgbvszyky2gmvyy8cq1fcqsnj9c92mbsv81g9qh")))
    (package
      (inherit base-rgbds)
      (license (license:non-copyleft "file://LICENSE.md")))))

(define-public rgbds-0.3.3
  (let ((base-rgbds
          (rgbds-with-version
            "0.3.3" "1dsw01ylbfqjbwv13n6yxjyakqmlfsvmlzv6h83df5mpix6mjfxv")))
    (package
      (inherit base-rgbds)
      (license (license:non-copyleft "file://LICENSE.md")))))

;; Skipping 0.3.4 and 0.3.6: potentially silent bugs mentioned in release notes,
;; and the releases are short-lived.

(define-public rgbds-0.3.5
  (rgbds-with-version "0.3.5" "1wxrvqrwg72rys4jhsk7id7lmvv40gw6xpn4sg29f8alfpc2bsry"))

(define-public rgbds-0.3.7
  (rgbds-with-version "0.3.7" "1bj082zi8lxrkkbsg5kvx6k1hkl156pqbxpblpidamk6qxqyssb9"))

(define-public rgbds-0.3.8
  (rgbds-with-version "0.3.8" "0db37z886026svhj6qnc3wk56sndbnz1vi41gn2k3bl6ppbnjlpk"))

;; Skipping 0.3.9 - from release notes:
;;   This version is missing definitions of the `__RGBDS_MAJOR__` etc. symbols.
;;   Please use 0.3.10 instead, which rectifies this problem (and does not
;;   introduce additional changes).

(define-public rgbds-0.3.10
  (rgbds-with-version "0.3.10" "0752fbffxgxyf3jw2iij88l05dqhppgcxy7dvk82hp4wdg4cflpq"))
