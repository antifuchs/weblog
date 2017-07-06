---
date: 2006-11-17T23:24:46Z
mt_id: 61
title: Threads for SBCL on FreeBSD
atom_id: http://boinkor.net/archives/2006/11/threads_for_sbcl_on_freebsd
---

I [don't use FreeBSD anymore](http://boinkor.net/archives/2005/06/anger_management_2.html), but [for some reason](http://www.ircbrowse.com/channel/lisp/20030713?utime=3267097749#utime_requested), this topic still interests me.

So it brings me great joy to see that NIIMI Satoshi [has a patch that adds threading support to SBCL on FreeBSD](http://article.gmane.org/gmane.lisp.steel-bank.devel/7915)! I tested it on FreeBSD 6.1-RELEASE running in Parallels Desktop for Mac, and all tests passed 2 of 3 times. I think this would be a good time for all FreeBSD-using lispers (especially those running it natively) to help us test this change.

Good hunting!
