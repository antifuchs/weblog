---
categories: ["Lisp"]
date: 2009-03-29T13:00:14Z
mt_id: 94
title: 'Clojure and Art of Illusion: BFF'
url: /2009/03/29/clojure_and_art_of_illusion_bf/
---

Two weeks ago, [the rotary program dial on my dishwasher broke](http://www.23hq.com/antifuchs/photo/4075448). Luckily, I could fix it with two Lego parts ([a Cross Axle #6 and a 16-teeth gear](http://www.ihmc.us/lego/kits/peices.html)) initially, but the program selection experience suffered from the axle's being too thin: it'd always bend in the turning direction until the selector would too-rapidly rotate past the point I wanted to select. Ugh.

Luckily for me, there's a [reprap](http://reprap.org/bin/view/Main/WebHome) at the [Metalab](http://reprap.soup.io). This is a low-cost 3d printer that can extrude parts made of ABS (the same material that Lego pieces are made of). With the help of [Philipp](http://metalab.at/wiki/Benutzer:Wizard23)'s MetaCADEvaluator plugin to [Art of Illusion (AoI)](http://aoi.sf.net), I managed to create a very nice 3d model of a replacement for the dial. That plugin let me define parameters for each part, so I could easily resize all parts manually once I discovered that I'd mistakenly noted down each part's diameter instead of the radius.  However, this was slightly fiddly business: All editing happens inside Art of Illusion's part name text fields, the syntax is slightly odd, and you can't define your own part library. 

Enter my urge to try out [Clojure](http://clojure.org). This little side project took three steps:

1. Find out how to embed clojure in AoI (done).
1. Make it open a swank port so I can on AoI from emacs without having to recompile all the time (done).
1. Build a part definition DSL (ongoing, one milestone achieved).

There's a detailed description of these steps after the jump. Here's [the github project for my AoI clojure plugin](http://github.com/antifuchs/aoi-swank-plugin/).

 

## Embedding clojure inside AoI

This was rather easier than I'd thought. The hardest part was finding out how clojure's [gen-class](http://clojure.org/compilation) works so as to generate two classes, one to implement the Plugin interface (so that the plugin gets loaded) and one to implement the Tool interface (so I get a menu entry that lets me start the Swank listener).

After that, it was building an extensions.xml and figuring out how to make ant build a .jar file that AoI could grok (both easy).

## Making it open a Swank port

That one was easy, as well: Just add the swank-clojure project as a submodule, and add its sources to the plugin .jar file. Having done that, open a port and add the current window somewhere so we can manipulate objects in it later ([source](http://github.com/antifuchs/aoi-swank-plugin/blob/1987fb6d3477fc5835d1c37b2762ac2cf82c5696/src/org/reprap/artofillusion/SwankTool.clj#L24)).

So after that, opening the Swank port vie the tool menu let me connect to AoI with emacs and off I went, doing experiments! For all who want to build Art of Illusion plugins with clojure, I've made a minimal-plugin branch that does exactly this, available [here](http://github.com/antifuchs/aoi-swank-plugin/tree/minimal-plugin).

## Building a part definition DSL

This one was the biggie: I wanted to write lisp that lets me interactively define 3d models. What is here right now is a little DSL that lets me create [simple 3d models](http://github.com/antifuchs/aoi-swank-plugin/blob/1987fb6d3477fc5835d1c37b2762ac2cf82c5696/src/org/reprap/artofillusion/objects.clj#L165-171), and lets me perform boolean operations on them ([union, difference, intersection](http://github.com/antifuchs/aoi-swank-plugin/blob/1987fb6d3477fc5835d1c37b2762ac2cf82c5696/src/org/reprap/artofillusion/objects.clj#L156-163)). This is enough to make [this model](http://github.com/antifuchs/aoi-swank-plugin/blob/1987fb6d3477fc5835d1c37b2762ac2cf82c5696/examples/dishwasher-part.clj), which is the [exact same part we extruded before](http://reprap.soup.io/post/15859377/Reprapping-a-Dishwasher-Replacement-Knob).

There are still things left TODO:

* The syntax of the DSL needs more love. It is quite verbose right now.
* Models need relative transformations: witness the "90 0 0" on almost every part.
* For debugging purposes, it would be useful to get a tree of the composite objects (with only the end result visible), instead of just the one result object.

## Conclusion

All of this was way easier and far more fun than I'd thought building plugins to java projects could be: I'd gone in expecting something in the order of gratuitously frustrating and mind-numbingly boring. Instead, after I had the first few hurdles out of the way (most of which were rooted in my emacs's slime config (-:), I was euphoric from regular small successes all the way. As you can tell from the [commit history](http://github.com/antifuchs/aoi-swank-plugin/commits/master), this took a little over 3 days to build.

I strongly recommend the Clojure and swank-clojure approach to building plugins to java projects: Once you've got a Swank listener open, it's all experimentation and small bits of progress. Excellent stuff, all around.
