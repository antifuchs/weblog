--- 
layout: post
title: "SBCL cvs->arch service turned off: use git."
mt_id: 46
date: 2006-10-23 10:31:42 -07:00
---
As [mentioned on the sbcl-devel mailing
list](http://article.gmane.org/gmane.lisp.steel-bank.devel/7710), I
have turned off syncing between the [SBCL](http://www.sbcl.org) CVS
repository and the [SBCL Arch
archive](http://boinkor.net/SBCL-in-arch.html).

The arch archive is still reachable, it just won't show any new
patches in future. If you would like to volunteer to continue the
repository syncing service for the cvs->arch, I'll happily turn over
my hacked tla and cscvs source trees, database, and conversion
scripts.

If you still used the Arch archive and would like to continue using a
patchset-ified full history of the SBCL repository, I suggest you try
out the [SBCL git repository](http://sbcl.boinkor.net/gitweb) (get it
via `git clone git://sbcl.boinkor.net/sbcl.git/`). It's both
faster and more functional than the arch archive, and has conversion
tools that work really well.

**Update:** the git repository url was wrong. Please use `git://sbcl.boinkor.net/sbcl.git/` or `http://sbcl.boinkor.net/git/sbcl-beta.git/` if you tried to download from the old url before. 
