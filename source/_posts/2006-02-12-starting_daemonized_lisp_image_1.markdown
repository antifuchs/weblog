---
layout: post
title: Starting daemonized lisp images in debian
mt_id: 40
date: 2006-02-12 13:46:39 -08:00
---
On baker, I'm running two lisp images: [autobench's web
interface](http://sbcl-test.boinkor.net/bench/), and a chavatar
instance for my girlfriend who is currently in St. Petersburg.

Starting these lisp images manually using detachtty is somewhat
tedious, so I wrote/modified two scripts to aid me with starting/attaching
to/starting swank in/stopping these
images. [lisp-images](http://boinkor.net/lisp/startup/lisp-images) is
an init.d script that starts one or all lisp images that are defined
in /etc/lisp-start.d/ and the other,
[start-stop-lisp-image](http://boinkor.net/lisp/startup/start-stop-lisp-image)
does the actual work. Note that they need start-stop-daemon to work;
I've tested them on debian, and I'm not sure about availability of
start-stop-daemon on other distributions, so you may need a debian-based
distribution to run them.

To install the scripts, put start-stop-lisp-image into /usr/local/sbin
and lisp-images into /etc/init.d, with appropriate permissions.

Also, you need a patched
[detachtty](http://www.cliki.net/detachtty). Marco Baringer's patch
for the --eval option is on the [detachtty bug
page](http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=282640). `apt-get
source detachtty`, patch, `fakeroot debian/rules binary` and `dpkg -i
../detachtty*.deb` was all I had to do on my machine.

Defining a lisp image to run is pretty easy: You drop a file into
/etc/lisp-start.d/ that is `source`d by the lisp image control shell
script.

A typical lisp image definition (for my chavatar instance) looks like
this:

	# The user to whom to switch before running the lisp image
	RUN_AS="asf"

	# these paths must be absolute:
	# Base directory; we chdir to it before running the lisp
	ABROOT="/home/asf/dl/svn/chavatar"
	# Directory where detachtty places its pipe and pid file
	VARROOT=$ABROOT/+var
	# Directory where detachtty dribble output goes
	LOGROOT=$ABROOT/+log

	# The command that we use to run the lisp.
	LISP_LOAD="/home/asf/bin/sbcl --noinform --userinit $ABROOT/+web-userinit.lisp"

	# The port where we start swank on demand.
	SWANKPORT=4008

Give it a descriptive name, and you're ready to roll. run
`/etc/init.d/lisp-images start image-name` and you should see:

	Starting Lisp images: image-name.

To attach to the lisp, run `/etc/init.d/lisp-images attach image-name`.

Init.d operation is easy, too: /etc/init.d/lisp-images operates on all
lisp images, if no images are given on the command line. This means
that you can create symlinks in `/etc/rc*.d/` and the images will be
automatically started on startup and stopped on shutdown.

If you want more information on how to use this setup, once it's
working, see [Bill Clementson's summary of nice things to do with lisp
webapps](http://bc.tech.coop/blog/050608.html).

Credits for these scripts go to Marco Baringer who wrote the attachtty
patch and the original ucwctl, from which most of
start-stop-lisp-image's meat is copied; and to Helmut Eller, not only
for the awesome SLIME, but also for figuring out that sbcl in
detachtty has no `*debug-io*`, which prevented the scripts' swank functionality
from working.
