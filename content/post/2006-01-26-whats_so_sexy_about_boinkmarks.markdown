---
date: 2006-01-26T19:49:21Z
mt_id: 39
title: What's so sexy about boinkmarks?
url: /archives/2006/01/whats_so_sexy_about_boinkmarks.html
---

Xach on #lisp asked me why people would want to run autobench to repeatedly build & benchmark sbcl. That's a good question, and I think I found a few answers:

* It gives you a build archive of previously built SBCL implementations, which you can use to run historical regression tests on. This is pretty nice to have: Autobench works using changesets, so you can often pinpoint exactly between which changes a feature that you want to use broke. I have been thinking about automating this process: pass it a test, and two revisions between which you suspect the test broke and it'll run the test for every revision in between.
* Autobench doesn't exclusively work when benchmarking sbcl. While it was developed specifically for sbcl, it's pretty easy to extend it to autobuild/benchmark your favourite lisp implementation. An incomplete port to CMUCL (no autobuilding) is already done. CLisp and ABCL would be interesting targets, as they're easy to build automatically, and they're not based on python.
* It gives you a chance to see how well your lisp/os/machine performs relative to mine (baker is an amd64 running linux) (:
* You don't want to have your sexy new machine (maybe even a rare architecture running a strange OS) sit around with a 0.x system load while it could do something useful instead.
