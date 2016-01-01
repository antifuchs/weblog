---
categories: Lisp
comments: true
date: 2012-09-09T00:00:00Z
title: Some more updates
url: /2012/09/09/some-more-updates/
---

So I've been moving stuff off my 6 year old server to a machine hosted
in Germany lately. I hope to bring back Boinkmarks on it some day soon.
(Not in the way I
[brought back the git repos](http://boinkor.net/archives/2012/09/git-lives-again-somewhere-else.html),
though - no outsourcing for benchmarks!) (-:

There are a couple state changes in my projects that would not warrant
a blog post on their own, but I think as a whole are still something
to write home about:

* I'm retiring the [Jofr.li](http://jofr.li/) web site - it pretty
  much got obsoleted at birth by Twitter's URL shortening thing, and
  it was just a finger exercise anyway. You can still peek at the
  source if you want to get a feel for how I think a lisp redis-backed
  hunchentoot app could be structured.

* The [CXML-RPC](https://github.com/antifuchs/cxml-rpc) library's
  server part should now work in the newest Hunchentoot. In the
  process, I think I found a bug in CXML's
  [Klacks](http://common-lisp.net/project/cxml/klacks.html) parser
  under [CCL](http://clozure.com/clozurecl.html) - it [fails to decode &# entities](http://lists.common-lisp.net/pipermail/cxml-devel/2012-September/000582.html).

* In not so very lisp-related news, I am learning that keeping your
  server's configuration in [puppet](http://puppetlabs.com/) is a
  really great thing. I'd never done this for my own machines up until
  this point, but it definitely helps to have all the state
  re-bootstrapable in one repository. Makes it way easier to reason
  about system configuration ("Now where are these vhosts' www root
  directories again? What, they were behind a bind mount? What was I
  thinking?!" - these moments are severely reduced when you can just
  look at git log output).

And lastly, [Dan Weinreb died](http://lispm.dyndns.org/dlw). I wish I
had had more opportunities to work with him, chat with him and learn
from him.
