---
categories: ["Lisp"]
date: 2007-02-06T23:43:38Z
mt_id: 73
title: Unintended consequences
url: /archives/2007/02/unintended_consequences.html
atom_id: /archives/2007/02/unintended_consequences
---

[asdf-dependency-grovel](http://boinkor.net/archives/2007/01/make_depend_for_lisp.html) seems to be more of a hit than I'd anticipated. Not only have I received great feedback from several people, ignas on #lisp already found the first a use case for it that I hadn't anticipated.

`* ignas loves asdf-dependency-grovel`<br/>
`<antifuchs> ignas: it works for you? (:`<br/>
`<antifuchs> cool!`<br/>
`* antifuchs happy`<br/>
`* ignas just finished stripping` [`closure`](http://common-lisp.net/project/closure/)`'s sgml parser of all the unnecessary stuff by using it`

So, it turns out that not only can you use adg to merge two large systems into one, it's possible to split one system into smaller components, as well! Not too bad for a tool that makes use of (hopefully) unintended consequences in the CL specification.
