---
date: 2007-04-11T09:33:38Z
mt_id: 83
title: ILC07 the inspirational lisp conference
url: /archives/2007/04/ilc07_the_inspirational_lisp_c.html
atom_id: http://boinkor.net/archives/2007/04/ilc07_the_inspirational_lisp_c
---

The talks were good, but most of the interesting stuff at conferences happens in the coffee breaks, anyway. (-:

Here's a list of half-to-fully-baked ideas that I'm going to explore in the near future:

* First of all, the [CLAPPA, the CL APPlication Archive](http://clappa.boinkor.net/) (link may be down due to my hacking on it) project got funding. The [LispNYC](http://www.lispnyc.org) breakout group rated it high for google Summer of Code, but then [Marco Baringer](http://bese.it) told me [Clozure](http://clozure.com) would be interested in funding this. There was so much interest in fact, that they decided to go ahead and really fund this project, thus freeing one SoC slot for another project! (More on that later.)

* If one were to write a somewhat backwards-compatible pathname system for CL (ahem), one could do worse than try and do translation to CL-structured pathnames by putting a translator object into the :HOST slot of the CL pathname. There would still be murkiness w.r.t. merging and creation of new pathnames, but at least it'd have a slight chance of working. Hm.

* McCLIM's CLIM Listener doesn't yet have a remote interface to lisps like [slime](http://common-lisp.net/project/slime/) does. One thing that would be neat is real remote lisp communication via the SWANK protocol. The other thing is extending that protocol to transport presentations and graphics in addition to text, maybe similar to what display postscript does.

* McCLIM currently has a few problems when running in multiple application frames: Each frame has its own command loop with its own input context, so when an application in frame A decides to accept something, and the user clicks on an object of that type in frame B, it's not entirely clear into which input context the object should go (and if it should be highlighted/clickable at all). An idea was to have an input context switcher widget that knows about the input contexts of all active application frames.

* asdf-dependency-grovel should separate dependencies at compile time and ones at load time. This sounds like it's easy to do, but I'm sure there are going to be surprises.

* I want an [Azul](http://www.azulsystems.com/products/compute_appliance.htm) machine to play with. They sound horrendously over-powered, but I bet they're the best platform to run [ABCL](http://armedbear.org/abcl.html) on. (-:
