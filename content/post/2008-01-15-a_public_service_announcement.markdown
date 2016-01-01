---
categories: ["Lisp"]
date: 2008-01-15T16:15:06Z
mt_id: 92
title: A public service announcement
aliases:
- /archives/2008/01/a_public_service_announcement.html
atom_id: http://boinkor.net/archives/2008/01/a_public_service_announcement
---

(This blog entry is **outdated**. The currently recommended way to get lisp software and its dependencies is [quicklisp](http://quicklisp.org/)).

## Outdated information

It has been brought to our attention that [asdf-install](http://www.cliki.net/ASDF-Install "CLiki : ASDF-Install") is still thought to be the preferred way to install cool lisp software. I would like to use this space to advertise an alternative tool that too few people know about, and that allows you to almost instantly (OK, as fast as your computer can install the required software and download & build the packages) get you up and running with and get you updates of the newest in cool lisp packages.

That tool is [clbuild](http://common-lisp.net/project/clbuild/).

It doesn't yet bring in all of the software available on cliki, but it includes enough cool things that I would recommend it to anyone who wants to check out with a minimum of hassle either of (not an exhaustive list, but you get the idea):

 * McCLIM,
 * Climacs, Closure, Beirc, Gsharp,
 * Hunchentoot, and
 * CXML

Or just get a lisp system up and running that includes most of the useful libraries out there. If you have been messing around with asdf-install (raise hands if you ever asdf-installed a library to get its dependencies and then pulled that library from CVS again and replaced the symlinks manually), do yourself a favour and check it out.
