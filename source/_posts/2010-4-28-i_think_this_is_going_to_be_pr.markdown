--- 
layout: post
title: I think this is going to be pretty sweet.
mt_id: 102
date: 2010-04-28 19:51:47 -07:00
categories: Lisp
---
{% img /assets/images/clucumber-baby-steps.png Clucumber baby steps %}

I've been toying around with making a Common Lisp adapter to [Cucumber](http://cukes.info/), a behavior-driven development tool. I think this will really be very sweet.

Here's how the step definition file would look like (these are really just stubs; in reality, you'd put in the lisp code you want to happen for the given textual description):

<pre>
(Given* #?"^I start clucumber in (.*)$" (path)
  (assert path))

(When* #?"^I define some-other-package as the test package$" ()
  (pending))

(Then* #?"^the current package should be \"([^\"]+)\"$" (package-name)
  (pending (format nil "package is ~A" package-name)))
</pre> 
