--- 
layout: post
title: CLISP boinkmarks available
mt_id: 55
date: 2006-11-08 15:11:18 -08:00
---
Here's the explanation of [yesterday's cryptic announcement](http://boinkor.net/archives/2006/11/this_will_probably_mean_nothin.html):

Prompted by a [late-night discussion on #lisp](http://www.ircbrowse.com/channel/lisp/20061103?utime=3371840108#utime_requested), I added build/benchmark support for CLISP to [autobench](http://boinkor.net/weblog/mt.cgi?__mode=view&_type=entry&id=38&blog_id=2). It's currently churning away at early releases, but some numbers are already in.

Graphs are available for [AMD64 in 32-bit mode](http://sbcl.boinkor.net/bench/?HOST=baker&IMPLEMENTATIONS=CLISP%2C%28%3AARCH+%3AEMULATED-X86%29&ONLY-RELEASE=NIL) and [AMD64 in native mode](http://sbcl.boinkor.net/bench/?HOST=baker&IMPLEMENTATIONS=CLISP%2C%28%3AARCH+%3AX86_64%29&ONLY-RELEASE=NIL).

When everything is working correctly, I'll start benchmarking CVS commits as they trickle in (just like I do for SBCL). If any volunteers want to dedicate hard disk space (the git tree is 186 MB, plus 1.5MB for every revision in the build archive) and processing power (building one revision takes around 10 minutes, 3 benchmark runs take 27 minutes), that would be much appreciated. See the [updated autobench setup guide](http://boinkor.net/archives/2006/01/sbcl_autobenching_aka_boinkmar.html) for details; if you need more convincing, I have a [flyer that highlights the advantages of running boinkmarks](http://boinkor.net/archives/2006/01/whats_so_sexy_about_boinkmarks.html) for you. 
