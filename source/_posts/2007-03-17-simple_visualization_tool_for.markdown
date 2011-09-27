--- 
layout: post
title: Simple visualization tool for string search
mt_id: 77
date: 2007-03-17 01:02:22 -07:00
categories: Lisp
---
I recently discovered that several of my (programming) friends know no string search algorithms other than the naïve left-to-right, one character at a time scan. There are much better algorithms out there, among them the one by [Boyer-Moore](http://www.cs.utexas.edu/users/moore/best-ideas/string-searching/). 

When I first heard about Boyer-Moore, it made me realize how easy it is to overlook opportunities for optimization -- I hadn't thought it feasible to speed up string search, either.

In order to allow others to see the beauty of it, I made a little CLIM visualizer app for string search algorithms, to use in a little intro session to good string matching code. Get the code [here](http://paste.lisp.org/display/38245), load it with McCLIM installed (preferably through <a href="http://lukego.livejournal.com/2530.html">clbuild</a>), and run `(run-frame-top-level (make-application-frame 'visualizer))`.
