---
categories: ["Lisp"]
date: 2007-03-17T01:02:22Z
mt_id: 77
title: Simple visualization tool for string search
atom_id: http://boinkor.net/archives/2007/03/simple_visualization_tool_for
---

I recently discovered that several of my (programming) friends know no string search algorithms other than the naïve left-to-right, one character at a time scan. There are much better algorithms out there, among them the one by [Boyer-Moore](http://www.cs.utexas.edu/users/moore/best-ideas/string-searching/).

When I first heard about Boyer-Moore, it made me realize how easy it is to overlook opportunities for optimization -- I hadn't thought it feasible to speed up string search, either.

In order to allow others to see the beauty of it, I made a little CLIM visualizer app for string search algorithms, to use in a little intro session to good string matching code. Get the code [here](https://gist.github.com/antifuchs/f26f604dfee1e925caaa3aba8d801dca), load it with McCLIM installed (preferably through <a href="https://www.quicklisp.org/">quicklisp</a>), and run `(run-frame-top-level (make-application-frame 'visualizer))`.

---

**Edit 2017-11-19**: moved the source code to gist (as paste.lisp.org was disabled due to abuse of the service; the original link went [here](http://paste.lisp.org/display/38245)); updated the install prerequisite instructions to point to quicklisp.
