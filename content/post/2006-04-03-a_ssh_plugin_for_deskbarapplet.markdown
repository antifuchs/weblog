---
date: 2006-04-03T16:19:24Z
mt_id: 44
title: A ssh plugin for deskbar-applet
url: /archives/2006/04/a_ssh_plugin_for_deskbarapplet.html
atom_id: /archives/2006/04/a_ssh_plugin_for_deskbarapplet
---

(No Lisp content in this one, move along.)

I've started using [Ubuntu](http://www.ubuntu.com/) GNU/Linux on my laptop, and I must say I rather like it. I was an [ion](http://www.modeemi.fi/~tuomov/ion/) user for a long time, and hadn't thought I would so readily accept the gnome desktop. Well, it's all working pretty smoothly, and with [deskbar-applet](http://raphael.slinckx.net/deskbar/) bound to F4, it feels a bit like ion, except that by default, it doesn't let you quickly open a terminal with a new ssh connection to a host.

Well, not anymore. Download [the deskbar plugin ssh.py](http://boinkor.net/misc/deskbar/ssh.py) into your ~/.gnome2/deskbar-applet/handlers/ directory, put the [ssh-to](http://boinkor.net/misc/deskbar/ssh-to) script into a directory on your PATH, select the "ssh" plugin in the deskbar preferences, and you're ready to go.

The plugin searches through non-encrypted entries in your ~/.ssh/known_hosts and Host entries in ~/.ssh/config. To open a SSH connection to a host, activate deskbar, type in a host name (`ssh://hostname` is also accepted) and select the "SSH to (hostname)" option. A new terminal window that contains your ssh connection should pop up.

Hint for ex-ion users like me: I made the SSH the highest-priority plugin and set the deskbar activation key to F4, so I have to type just F4, the hostname, and return. (-:
