Guix-Past: Bringing software from the past to the present
=================================================================
[![pipeline status](https://gitlab.inria.fr/guix-hpc/guix-past/badges/master/pipeline.svg)](https://gitlab.inria.fr/guix-hpc/guix-past/commits/master) [![SWH](https://archive.softwareheritage.org/badge/origin/https://gitlab.inria.fr/guix-hpc/guix-past.git)](https://archive.softwareheritage.org/browse/origin/https://gitlab.inria.fr/guix-hpc/guix-past.git)

So you want to reproduce the software environment that you used ten
years ago, say, as the basis of the conclusions of a scientific paper?

You would happily run [`guix
time-machine`](https://guix.gnu.org/manual/devel/en/html_node/Invoking-guix-time_002dmachine.html).
Alas, [GNU Guix](https://guix.gnu.org) didn’t exist in 2010.

Worry no more!  The *Guix-Past* channel brings old software to today’s
world.  It gives you packages from “back then” that you can deploy
here and now.

# Getting Started

To use it, adjust `~/.config/guix/channels.scm` so that it reads
something along these lines:

```scheme
(cons (channel
        (name 'guix-past)
        (url "https://gitlab.inria.fr/guix-hpc/guix-past")

        ;; The following lines allow 'guix pull' to authenticate
        ;; this channel.  It requires a recent Guix (July 2020)
        ;; and can be omitted with older versions.
        (introduction
         (make-channel-introduction
          "0c119db2ea86a389769f4d2b9c6f5c41c027e336"
          (openpgp-fingerprint
           "3CE4 6455 8A84 FDC6 9DB4  0CFB 090B 1199 3D9A EBB5"))))
      %default-channels)
```

… and run `guix pull`—see [the Guix
manual](https://guix.gnu.org/manual/devel/en/html_node/Channels.html)
for more information on channels.

Read the “Hacking” section for information on how to grow this channel!

# Pre-Built Binaries

Pre-built binaries for Guix Past packages are served from
`https://guix.bordeaux.inria.fr`.  To benefit from them, you must:

  1. Add `https://guix.bordeaux.inria.fr` to the `--substitute-urls`
     option [of
     `guix-daemon`](https://www.gnu.org/software/guix/manual/en/html_node/Invoking-guix_002ddaemon.html#daemon_002dsubstitute_002durls)
     or that [of client
     tools](https://www.gnu.org/software/guix/manual/en/html_node/Common-Build-Options.html#client_002dsubstitute_002durls).
     To enable it globally, do:
	 
	 ```
	 $EDITOR /etc/systemd/system/guix-daemon.service

	   … add ‘--substitute-urls='https://ci.guix.gnu.org https://guix.bordeaux.inria.fr'’
       to the ‘guix-daemon’ command line.
	  
     systemctl daemon-reload
	 systemctl restart guix-daemon.service
	 ```

  2. [Authorize](https://www.gnu.org/software/guix/manual/en/html_node/Substitute-Server-Authorization.html)
     the key used to sign substitutes:

	 ```
	 (public-key
	  (ecc
	   (curve Ed25519)
	   (q #89FBA276A976A8DE2A69774771A92C8C879E0F24614AAAAE23119608707B3F06#)))
	 ```
	 
	 Typically, assuming the key above is stored in `key.txt`, run as root:
	 
	 ```
	 guix archive --authorize < key.txt
	 ```

# Hacking

This channel provides exclusively free software in accordance with the
[FSDG](https://www.gnu.org/distros/free-system-distribution-guidelines.html).

The module hierarchy mirrors that of upstream Guix.  For example, the
`(past packages autotools)` module contains old versions of packages
found in `(gnu packages autotools)`.

To loosen coupling with Guix modules, it is sometimes a good idea to
resort to `specification->package` instead of relying on modules and
variable names, which are subject to change.

It makes most sense to add software dating back to before 2019, around
the time where [time travel became
possible](https://guix.gnu.org/blog/2018/gnu-guix-and-guixsd-0.15.0-released/).

That’s it!  Now you’re welcome to contribute your bits!

# Meta-History

This channel saw the light as part of the ReScience C [Ten Years
Reproducibility Challenge](https://rescience.github.io/ten-years/).
