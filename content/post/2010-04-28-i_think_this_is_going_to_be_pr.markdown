---
categories: ["Lisp"]
date: 2010-04-28T19:51:47Z
mt_id: 102
title: I think this is going to be pretty sweet.
additional_syntax:
- cl
atom_id: http://boinkor.net/archives/2010/04/i_think_this_is_going_to_be_pr
---

I've been toying around with making a Common Lisp adapter to [Cucumber](http://cukes.info/), a behavior-driven development tool. I think this will really be very sweet.
<!--more-->
{{%img alt="Clucumber baby steps" src="/assets/images/clucumber-baby-steps.png"%}}

Here's how the step definition file would look like (these are really just stubs; in reality, you'd put in the lisp code you want to happen for the given textual description):

```
(Given* #?"^I start clucumber in (.*)$" (path)
  (assert path))

(When* #?"^I define some-other-package as the test package$" ()
  (pending))

(Then* #?"^the current package should be \"([^\"]+)\"$" (package-name)
  (pending (format nil "package is ~A" package-name)))
```
