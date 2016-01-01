---
categories: ["Lisp"]
comments: true
date: 2011-12-21T00:00:00Z
title: IDNA Now Supports Punycode Decoding
url: /archives/2011/12/idna-now-supports-punycode-decoding.html
atom_id: http://boinkor.net/archives/2011/12/idna-now-supports-punycode-decoding
---

My [IDNA library](http://github.com/antifuchs/idna) now supports
decoding IDNA strings via the to-unicode function:

        (to-unicode "xn--mller-kva.example.com")
        ;; =>  "müller.example.com"

That's in addition to the regular encoding for unicode domain names:

        (to-ascii "müller.example.com")
        ;; => "xn--mller-kva.example.com"

Sadly, I haven't managed to get the logic for case-sensitive punycode
encoding to work yet. But fortunately, IDNA domain name encoding
doesn't require that! Anyone looking for some low-hanging fruit-shaped
lisp projects is welcome to add that! (-:
