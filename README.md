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
        (url "https://gitlab.inria.fr/guix-hpc/guix-past.git"))
      %default-channels)
```

… and run `guix pull`—see [the Guix
manual](https://guix.gnu.org/manual/devel/en/html_node/Channels.html)
for more information on channels.

Enjoy it, and [grow it](https://gitlab.inria.fr/guix-hpc/guix-past)!

# Meta-History

This channel saw the light as part of the ReScience C [Ten Years
Reproducibility Challenge](https://rescience.github.io/ten-years/).
