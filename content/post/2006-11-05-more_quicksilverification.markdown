---
date: 2006-11-05T19:24:31Z
mt_id: 53
title: More Quicksilverification
atom_id: http://boinkor.net/archives/2006/11/more_quicksilverification
---

Hacked on [the SSH Plugin](http://boinkor.net/archives/2006/11/hacking_objective_c_for_fun_an.html) some more. Now it can do:

* Connections to hosts that are not in the index. Type . (the period character) to enter Text mode, enter a host name and select the "Open SSH Connection" action.
* Connections to hosts with non-default user names. Use the "Open SSH Connection as User..." action for that.
* Oh, and there's some documentation!

I'm declaring this feature-complete. If you installed the previous, pretest version, you will have to remove the "SSH Plugin" before installing the new one: Open Preferences, Plug-ins, select the SSH Plugin and use "Delete selected Plug-ins".

Get the new version here: [DMG](http://boinkor.net/qs/SSH%20Plugin.dmg). As always, there's the [source (MIT licenced)](http://boinkor.net/qs/QSSSHPlugin.tar.gz) and [git repository](http://boinkor.net/qs/QSSSHPlugin.git/), too.
