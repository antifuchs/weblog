---
date: 2006-03-27T14:20:41Z
mt_id: 43
title: McCLIM 0.9.2 out-of-the-box demo application available
aliases:
- /archives/2006/03/mcclim_092_outofthebox_demo_ap_1.html
atom_id: http://boinkor.net/archives/2006/03/mcclim_092_outofthebox_demo_ap_1
---

Following [Christophe's suggestion](http://meme.b9.com/cview.html?channel=lisp&utime=3352443780#utime_requested), I have put up a binary of McCLIM 0.9.2, its demos, and the Inspector, Debugger and Listener applications.

As stated on the mailing list, it's based on a threaded SBCL 0.9.11 and requires Linux 2.6.x on an i386 machine. If you are on debian, it needs libfreetype6-dev installed, as well.

Get it at [http://boinkor.net/lisp/mcclim-listener-0.9.2.tar.bz2](http://boinkor.net/lisp/mcclim-listener-0.9.2.tar.bz2).

To try out McCLIM's applications, download the file (14MB), unpack it, and run the mcclim-listener-0.9.2/mcclim binary. It doesn't get much easier than that. (:

Some fun things to try:

on the Listener prompt, enter

        (/ 1 0)  ; to open the clim debugger

or activate the menu item "Demos -> Plot Fishes using Functional Geometry" to see a nice "Escher" style plot.
