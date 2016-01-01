---
categories: ["Hacks"]
comments: true
date: 2013-01-19T00:00:00Z
title: 'Elixir: First Impressions'
url: /2013/01/19/elixir-first-impressions/
---

For the longest time now, I've admired
[Erlang](http://www.erlang.org/) from afar. It always seemed to be a
bit daunting to take on. For one, there was the slightly weird and
inconsistent Prolog-inspired syntax (I was always scratching my head
over why *this* place needs a period and *that* place doesn't), and
then there was just plain weird stuff like one-based indexes.

While you don't end up needing indexes very often, a nice syntax on
top of Erlang is something I always kind of wanted, but nothing really
could deliver. Then I saw Jose Valim demoing
[Elixir](http://elixir-lang.org/) at
[Strange Loop 2012](https://thestrangeloop.com/archive/2012). It has a
ruby-inspired (but more regular) syntax, it can do
[macros](http://elixir-lang.org/getting_started/5.html)(!), it has
[protocols](http://elixir-lang.org/getting_started/4.html)(!!!), and
it has a very enthusiastic developer community behind it (see
[expm](http://expm.co/) for an example of the packages that people
have written/ported over to Elixir). That its data structures use
zero-based index access certainly helps, too (-:

On top of all these nice things, it also lets you use any Erlang
library (with only minimally less nice syntax by default). I think I'm
sold.

## What is all that hair on the floor?

As an initial just-for-fun project, I tried porting over the progress
I'd made on a node.js-based gmail->localhost IMAP backup tool that I'd
optimistically named gmail-syncer.[^1] So far, this has required a ton
of
[yak shaving](http://projects.csail.mit.edu/gsb/old-archive/gsb-archive/gsb2000-02-11.html),
but I'm enjoying the hell out of every single step down the fractal
yak ranch.

* First, there is no suitable IMAP client library. The thing that
  comes closest is [erlmail](http://code.google.com/p/erlmail/). It is
  somewhat abandoned, and its IMAP client isn't very usable for my
  purposes (doesn't implement capabilities the way I need them,
  doesn't really follow the
  [one relatively sane guide to writing an IMAP client](http://dovecot.org/imap-client-coding-howto.html)). So
  I'll have to write my own IMAP interaction code.

* To write my own IMAP code, I need to parse server responses; this
  requires parsing the highly weird IMAP protocol, with its somewhat
  lisp-inspired (but definitely not lispy) ideas of how to represent
  things. For example, The way a UID FETCH response looks makes it
  pretty impractical to tokenize & parse the response using a parser
  generator - unless you enjoy concatenating potentially dozens of
  megabytes of text that would do better to remain as an opaque binary
  buffer.

* Hence, to parse server responses in a smarter way, I have to have a
  smarter parser. While that can use a pretty nice heuristic (despite
  its lispy nature, the IMAP server responses are specified to
  terminate in newlines at certain points), I still need it to
  cooperate well with something that manages buffers received from the
  network somewhat smartly. Aaaand that's where I am right now.

Introducing
[gmail_synchronize](https://github.com/antifuchs/gmail_synchronize),
the tool that doesn't do very much right now other than fill a buffer
and let you read lines or N-byte-long binaries from them. But I'm sure
there will be more stuff eventually (-:

To come this far, I've written some kilobytes of code (on various
levels of the aforementioned yak stack) and thrown them away. The
results in the git repo are the best I have come up with, so far. This
isn't much, and so you should take the following opinions with a mine
of salt.

## My impression of Elixir so far

Here's a brain dump of what about the language stood out to me:

So far, I really like Elixir (and, by extension, Erlang). There's a
lot to be said about its pattern matching (which is as powerful as
[Erlang's](http://learnyousomeerlang.com/syntax-in-functions)), but I
don't think I fully understand it yet. There's a bit of terminology I
still have to learn, but even at this level of (non-)proficiency, it's
making my job way easier.

There's a very helpful channel on freenode,
[#elixir-lang](irc://irc.freenode.net/#elixir-lang). It has the
creator of the language in it, and a bunch of very enthusiastic,
knowledgeable and helpful people (hi, yrashk and cmn!). This has been
invaluable in my learning to use the language.

I still don't quite get why some of the decisions in it were made the
way they were made. For example, it would seem natural to me to have a
way to pattern-match binary buffers to test whether some bytes appear
next to each other in the buffer, but there isn't. I guess this may
have to do with being able to unambiguously resolve the pattern, but
it's still a bit unsatisfactory. I'm sure this will pass as I learn
more of its vocabulary and integrate it into mine.

Testing in Elixir is very cool. Instead of mocking or stubbing things
like I would in, say, Ruby, I factor things such that tests can
implement a protocol that the part being tested uses, and I'm set. I
love
[protocols](http://www.amazon.com/Art-Metaobject-Protocol-Gregor-Kiczales/dp/0262610744),
and I think Elixir lets you use them in a very nice way. See
[here](https://github.com/antifuchs/gmail_synchronize/blob/77fbca779588a0d92f0a18395e4149ff309722df/test/network_test.exs#L8-L25)
for how the tests interact with a library that follows a
protocol. Note the re_buffered variable - in Ruby, I'd be using a
[method call expectation](http://gofreerange.com/mocha/docs/Mocha/Expectation.html#times-instance_method)
instead - this is way more satisfying.

Non-modifiable data structures are way less of a pain than I'd
imagined (they are in fact pretty pleasing). The pattern matching
makes things much easier to follow, and the way updates (which return
a new object) work is also pretty cool: You can write stuff like:
``` ruby
some_record.buffer("foo").number(20)
```
...and this returns a record that is like some_record, except its `buffer`
and `number` components are replaced by the values passed in the
function argument list. Pretty pleasing.

I would not have been able to write code so relatively painlessly if
it weren't for the
[emacs mode](https://github.com/antifuchs/elixir-mode) that I've
painfully adjusted to automatically indent Elixir code
correctly. Emacs's
[smie](http://www.gnu.org/software/emacs/manual/html_node/elisp/SMIE.html)
is really pretty cool, and I wish more emacs modes used it (-:

That's all so far. I urge you to check out Elixir, and hope you have
as much fun with it as I do!

[^1]: Why write a new tool over using [offlineimap](http://offlineimap.org/)? Offlineimap is a huge pain - when used with gmail, it'll sometimes run into UIDVALIDITY mismatches (which require a re-download of potentially huge mailboxes, which run for days), it's slow, and its thread-based design is so horrible that it manages to mess up its own UI even when using a single worker thread, and then it can't even exit cleanly on anything other than a SIGKILL. Arrrrgh.
