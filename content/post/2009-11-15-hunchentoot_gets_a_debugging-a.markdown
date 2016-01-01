---
categories: ["Lisp"]
date: 2009-11-15T22:11:36Z
mt_id: 99
title: Hunchentoot gets a debugging-acceptor
url: /archives/2009/11/hunchentoot_gets_a_debugging-a.html
atom_id: http://boinkor.net/archives/2009/11/hunchentoot_gets_a_debugging-a
---

Today, I submitted a patch (the first free software lisp one in months for me!) to the [Hunchentoot](http://weitz.de/hunchentoot/) project, and it got accepted. Yay!

Some backstory: Hunchentoot's 1.0.0 release dropped a lot of implementation-dependent features, among them functionality to invoke the debugger if an error happens while handling a request. While [workarounds](http://paste.lisp.org/display/81046) [exist](http://www.lispworks.com/documentation/HyperSpec/Body/v_break_.htm), none of them were obvious to new users or users who recently upgraded.

The patch I sent should fix this, hopefully. It adds a rudimentary error handling protocol to Hunchentoot, and provides two generic functions whose behavior can be adapted to your error handling needs. You can see for yourself in Hunchentoot's [svn repository](http://bknr.net/trac/browser/trunk/thirdparty/hunchentoot).

If you're a Hunchentoot user, I urge you to test this (in both development mode using debuggable-acceptor and running with the default settings). The sooner you find bugs, the sooner they can be fixed, the sooner a release can be pushed out. And if you don't find bugs at all, that's cool, too (-:
