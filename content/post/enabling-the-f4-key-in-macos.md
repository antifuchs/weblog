---
Categories: ['Hacks']
Description: ""
Tags: []
date: 2017-11-19T16:00:00-07:00
title: Enabling the F4 key in macOS
---

This problem has been a mystery to me, and I figure to [a bunch](https://forums.macrumors.com/threads/f4-launchpad-key.1215005/) [of other people](https://discussions.apple.com/thread/7146702?start=0&tstart=0), too: If you hit F4 in Mac OS X (or macOS) since Lion, it does not have any effect. What.
<!--more-->
It appears that the key (when hit without modifiers) is disabled for some reason: I mainly rely on the Function keys on my mechanical keyboard to switch windows in [tmux](https://github.com/tmux/tmux/wiki), and e.g., if you hit shift-F4 (the same thing, according to the terminal), it actually works.

There's a bunch of forums that advise deleting `~/Library/Preferences/com.apple.symbolichotkeys.plist`, which also removes all your custom app shortcuts. I have a bunch of those, and would prefer to keep those, thank you!

Turns out you can not do that and still get the desired behavior:

## A milder fix

The main insight that led me to this fix is outlined on [this post](http://krypted.com/mac-os-x/defaults-symbolichotkeys/)[^unattributed]: The symbolic hotkeys plist is a mapping of key codes to some parameters. So, after some experimentation, I cooked up this command line (which, if you try it, make sure you create a backup of the `~/Library/Preferences/com.apple.symbolichotkeys.plist` file!)

``` sh
defaults write ~/Library/Preferences/com.apple.symbolichotkeys.plist AppleSymbolicHotKeys -dict-add 96 '{enabled = 1; value = {parameters = (96); type = standard; }; }'
```

This, I think, does the following: It adds key `96` to the plist (96 stands for F4, according to the krypted blog post), with a parameter that I can only guess makes it send the 96 keycode (and if it doesn't, at least doesn't do harm), as a "standard" key, and enables that key.

After logging out and back in, pressing my F4 key unmodified works, and all my custom app shortcuts are still there. Win!

Do let me know if this works for you!

[^unattributed]: This post does not have any attribution on it, but it appears that it is written by Charles Edge. Thanks, Charles!
