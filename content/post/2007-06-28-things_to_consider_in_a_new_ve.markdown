---
categories: Lisp
date: 2007-06-28T01:05:18Z
mt_id: 86
title: Things to consider in a new version of ASDF
url: /2007/06/28/things_to_consider_in_a_new_ve/
---

[ASDF](http://cliki.net/asdf)([reference manual](http://constantly.at/lisp/asdf/)) is a fine piece of software: I like to use it, and find some of the design choices in it to be very good (besides, it's the thing that has allowed things like [clbuild](http://common-lisp.net/project/clbuild) and [asdf-install](http://www.cliki.net/ASDF-Install "CLiki : ASDF-Install") to grow). However, it is now in wide enough use that people are looking at its internals and are scratching their heads when finding some not-so-very-good design choices.

Here's a selection of the things that I think would be good candidates for review in a new version of ASDF (bsdf? yasdf? qwer? /.,m?):



* Using `TRAVERSE` to come up with a plan for execution (and executing this plan in `OPERATE`) isn't so great: it doesn't allow modules' `PERFORM` methods to wrap their children's `PERFORM` methods (for stuff like `with-compilation-unit` or muffling warnings), and makes it quite painful to operate on a non-sequential plan for execution.

* Component references (e.g. in :depends-on lists) shouldn't be stored by name, but refer to the component objects themselves. Right now, ASDF stores the component name only, so you always have to look up the component in the enclosing module. AIUI, this is the primary reason why ASDF doesn't support rebuilding of files that depend on files in different modules modules([see](http://boinkor.net/archives/2007/01/explaining_some_features_of_as.html)).

 Doing it this way is a bit tricky, as depended-on components might not have been constructed yet: I suppose you could do this via prototypes and replace them later on.

* Speaking of :depends-on, it should really be up to the operation and the component to decide what something on the :depends-on list means, not to `ASDF::PARSE-COMPONENT-FORM`. I think something like

``` cl
defmethod component-dependency-meaning (operation component depended-on-component)
```

 should work. Oh, and drop [do-first](http://article.gmane.org/gmane.lisp.cclan.general/674). (-:

* It seems everybody is aching for versioned dependencies. AFAICT, if you do that, you need a way to compare versions and specify "I want version X or newer, but nothing younger than Y" things, and it would also be nice to be able to say that system A conflicts with version Y of system B. I think of Debian's .deb format, and think this would be ideal.

 Of course, this whole mechanism won't be of any use unless there are people who use it and keep the information in good shape. This would be the topic of several more blog posts, and is the reason why I'd suggest leaving versioned dependencies out of the design of a new ASDF, and instead making the component form parser and normalizer smart enough to cope with them once they are implemented in a sensible way.

* Some sense of backwards compatibility (note that the changes proposed here are somewhat incompatible) or a means of easily converting old .asd files to .**(whatever the new name is)** ones would be nice. And being able to depend on .asd systems from **(whatever the new name is)**.

So. I'm not sure if a blog post is the correct medium for this, but I somehow find this more suitable than a mail to cclan-devel. If you've got comments, do blog them yourself or send me an e-mail. (*Disclaimer:* This is what I consider important. More ASDF-wise people might have more insight into this topic. I've just been thinking about them for quite some time, and just recently came up with the right words.)
