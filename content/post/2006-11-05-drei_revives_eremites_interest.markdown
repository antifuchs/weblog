---
date: 2006-11-05T01:51:50Z
mt_id: 52
title: DREI Raises Engineer's Interest
url: /2006/11/05/drei_revives_eremites_interest/
---

[Troels Henriksen](http://sigkill.dk/blog/) has [made available](http://article.gmane.org/gmane.lisp.mcclim.devel/1216) his modifications to McCLIM: He added an input editor (named "DREI") that is based on [climacs](http://common-lisp.net/project/climacs/). The greater plan seems to be to make Climacs use that input editor for all its editing panes.

Apart from less code duplication and embeddability of a really nice emacs in every clim application that uses a text-editor gadget, this adds a few cool things to the input editing in current clim applications. This screen shot

![A clim listener with an active DREI input editor](http://boinkor.net/lisp/porn/DREI-Listening.png)

shows:

* Lisp syntax highlighting,
* (almost) correct indentation of forms, and
* nicely formatted "noise" strings: *(pathname)*. 
