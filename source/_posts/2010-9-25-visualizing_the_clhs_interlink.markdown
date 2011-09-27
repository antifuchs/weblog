--- 
layout: post
title: Visualizing the CLHS interlinking structure
mt_id: 104
date: 2010-09-25 00:52:02 -07:00
categories: Lisp
---
Lately, I've been wondering what we could use the ~110k Hyperlinks in the CLHS for, other than click through the spec in a web browser. For example, given a glossary entry, how do you find out which functions refer to it?

So I wrote a little program that crawls (a local copy of) the HyperSpec, and creates an RDF graph of its link structure. This graph can be used to answer these questions, or it could be the basis of a useful research overlay on top of the HyperSpec, who knows.

Lots of tools exist to visualize and manipulate the facts in an RDF graph already; Here's a screen shot of [gruff](http://www.franz.com/agraph/gruff/) with a small trace through the HyperSpec:

{% img /assets/images/gruff-with-hyperspec-rdf.png Gruff with Hyperspec RDF %}

You can get the code to create the RDF graph [at github](http://github.com/antifuchs/clsem/). 
