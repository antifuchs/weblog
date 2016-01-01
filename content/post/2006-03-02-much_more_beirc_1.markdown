---
date: 2006-03-02T23:09:04Z
mt_id: 41
title: MUCH MORE BEIRC
url: /archives/2006/03/much_more_beirc_1.html
atom_id: http://boinkor.net/archives/2006/03/much_more_beirc_1
---

While [several](http://www.lisphacker.com/) [others](http://cyrusharmon.org/cl/blog/) are busy porting [sbcl](http://www.sbcl.org) to [platforms](http://www.microsoft.com/windows/default.mspx) [that God itself hates](http://www.apple.com/imac/), I've been hacking on something much more trivial, but much more fun (and rewarding, in the short term): [beirc][b].

Some recent changes:

* Multi-server support! You can now be connected to more than one server at the same time, and be in many channels (with the same names, even) on every one of them, all in one beirc instance.
* Query renaming: If a user changes his or her nick, and you have a query window open, that query window will be renamed and messages to that user will go to their new nickname. Note: due limitations in the irc protocol, this will only work if you are in at least one common channel with that user.
* Keyboard shortcuts to switch tabs: ctrl-pgdown/ctrl-pgup switch to the next/previous tab and ctrl-tab and ctrl-shift-tab switch to the next/previous unread tab with unread content.
* Better Tab highlighting: Tabs now highlight only if there is genuinely interesting content (messages, operator actions or messages mentioning you) happening in the window while you're not watching.
* Generally Better Code (I think), which can deal with more of the stuff that cl-irc throws at it.

So, I think you definitely should have a look at [beirc][b]. Outside of work (where I have to use [irssi](http://www.irssi.org/) in a [screen]( http://www.gnu.org/software/screen/) instance), I use it exclusively now. It really is that good. I'm an IRC addict, you can trust me. (-:

[b]: http://common-lisp.net/project/beirc/
