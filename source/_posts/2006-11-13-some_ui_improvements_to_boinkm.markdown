--- 
layout: post
title: Some UI improvements to boinkmarks
mt_id: 56
date: 2006-11-13 20:14:38 -08:00
---
I've spent the last few hours adding a bit of javascript/araneida magick to [the boinkmarks web interface](http://sbcl.boinkor.net/bench/). If you have javascript enabled, and your browser is supported by [dojo](http://dojotoolkit.org), it's possible to select the hosts, implementations, releases that you want in a matter of seconds (and not in 2-3 minutes). Yay!

Also, that taught me some useful lessons:

* I'm not cut out for web development. Seriously, ew.
* [Parenscript](http://parenscript.org/) is nice. <strike>But then you find out you can't write `something.innerHTML = ...`, because there is no way for it to emit a variable name that consists only in *part* of capital letters -- and javascript is case-sensitive! Argh!</strike> You can get innerHTML by using (in lisp) something.inner-h-t-m-l. (thanks to Marco Baringer and Ivan Toshov)

In other news, [Postgresql](http://www.postgresql.org) = love. It's really a joy to search for performance problems with it. The last one I weeded out was this: 

When you join a small table to a big table in order to *limit* the number of resulting rows to ones matched in a big table, don't use JOIN, use WHERE EXISTS.

For example, if you want smalltable's foo column for rows that are related to those rows in bigtable that match bigtable.condition='something', don't use 

  `select smalltable.id, smalltable.foo from smalltable join bigtable on bigtable.id=smalltable.id where bigtable.condition = 'something'`
 
  , use: 
  
  `select id, foo from smalltable where exists (select * from bigtable where bigtable.id = smalltable.id and bigtable.condition = 'something' limit 1)`.

(And don't forget to keep an index on `(bigtable.id, bigtable.condition)`!) Rewriting sql statements like this (and using an index) gave the queries in the boinkmarks web app a speed boost of anything between 30x and 200x. 

It's still slow, but at least now I can't do anything obvious about it anymore. (-:
