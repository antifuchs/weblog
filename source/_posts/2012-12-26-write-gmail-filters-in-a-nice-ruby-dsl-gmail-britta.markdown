---
layout: post
title: "Write gmail filters in a nice Ruby DSL: gmail-britta"
date: 2012-12-26 19:16
comments: true
categories: Hacks
---
I've just finished (mostly) documenting and writing tests for my
latest little library, [gmail-britta][gb], so thought I should release
it to the world as a sort of holiday gift.

[Gmail-britta][gb] is a library that lets you write Gmail filters
(hah, Britta, get it?) in a way that doesn't drive you insane - you
write them as a ruby program, run that program, and out comes XML that
you can import into
[Gmail's filter settings](https://mail.google.com/mail/u/0/?shva=1#settings/filters).

It does a bunch of other nice things, but I guess it's better to let
the
[README explain](https://github.com/antifuchs/gmail-britta/blob/master/README.md)

So far, I (and a few colleagues of mine) have been successfully using
this for the past few months to generate filters for
[work email](http://blog.alexmaccaw.com/stripes-culture). Just
yesterday I took the step and ported my
[156 filters](https://twitter.com/antifuchs/status/283753876807614464)
over to a
[gmail-britta program (yep, that's my filters, with sensitive email addresses stubbed out)](https://github.com/antifuchs/gmail-britta/blob/master/examples/asf.rb),
resulting in
[34, easier to maintain, more accurate filters](https://twitter.com/antifuchs/status/283754273240674304).

If you're interested, please
[give it a try](http://rubygems.org/gems/gmail-britta). Also,
[please let me know in the issues](https://github.com/antifuchs/gmail-britta/issues)
if you find anything that it doesn't do, or if you're feeling super
generous,
[please open a pull request and send me improvements](https://github.com/antifuchs/gmail-britta/pulls)!

[gb]: https://github.com/antifuchs/gmail-britta
