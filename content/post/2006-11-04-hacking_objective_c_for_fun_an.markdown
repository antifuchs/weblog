---
date: 2006-11-04T19:29:26Z
mt_id: 51
title: 'Hacking Objective C for fun and profit: An SSH plugin for Quicksilver'
url: /2006/11/04/hacking_objective_c_for_fun_an/
---

In a [recent entry](http://boinkor.net/archives/2006/04/a_ssh_plugin_for_deskbarapplet.html), I compared [deskbar-applet](http://raphael.slinckx.net/deskbar/) with [Quicksilver](http://quicksilver.blacktree.net) and introduced a plugin I had written. Well, I now own a macintosh, and so I wanted Quicksilver to gracefully handle SSH connections, too.

Of course, it's possible to make a safari/mozilla/omniweb bookmark of the form `ssh://some-host/` and have quicksilver add them to its index, but that gets old quickly. I want all hosts in my known_hosts file to be available, and I don't want to type in all 23 of them - again. So I had to write my own SSH plugin, which turned out to be fun.

Quicksilver is pretty badly documented, but the kind people on [#quicksilver](irc://irc.freenode.net/#quicksilver) were very helpful -- thanks, ytrewq2! -- and so I managed to create a working plugin.

Here's a pretest version that reads in known_hosts (user and system-wide) and the ssh client config and makes them available as ssh URLs. I have some bigger plans with that, but for now this works well enough for me: [**updated** DMG](http://boinkor.net/qs/SSH%20Plugin.dmg) ([see](http://boinkor.net/archives/2006/11/more_quicksilverification.html)), [source (MIT licenced)](http://boinkor.net/qs/QSSSHPlugin.tar.gz), [git repository](http://boinkor.net/qs/QSSSHPlugin.git/)

Incidentally, this was my first contact with Objective C. It feels like a nice language, and the development environment ([Xcode](http://developer.apple.com/tools/xcode/)) is pretty awesome. There's very high-quality documentation for everything included -- except Quicksilver internals, of course. (-:
