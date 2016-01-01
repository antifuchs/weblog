---
categories: ["Lisp"]
comments: true
date: 2011-10-08T00:00:00Z
title: Accessing the Stripe API From Lisp
url: /archives/2011/10/accessing-the-stripe-api-from-lisp.html
atom_id: http://boinkor.net/archives/2011/10/accessing-the-stripe-api-from-lisp
---

[Stripe](https://stripe.com) is a new payment processor on the Web,
and they seem to be
[a lot less insane](http://www.google.com/search?q=paypal+screwed+me)
than Paypal. On a whim, I made a little (almost completely untested,
toy) CL library for accessing their HTTP API from Lisp. Maybe you'll
find it useful: [cl-stripe](https://github.com/antifuchs/cl-stripe).

This was pretty great fun! Thanks to their nice
[HTTP API](https://stripe.com/api/docs),
[drakma](http://weitz.de/drakma), and
[alexandria](http://common-lisp.net/project/alexandria/), I have been
able to write this with a minimum of horribly hacky code, in just 5 or
6 hours of working on it, on and off, this saturday afternoon.

If it still looks like fun, I think I may add some clucumber tests to
it tomorrow. Stay tuned.
