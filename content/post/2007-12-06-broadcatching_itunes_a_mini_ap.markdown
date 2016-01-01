---
date: 2007-12-06T01:18:37Z
mt_id: 91
title: Broadcatching, iTunes, a "mini" Apple TV
url: /archives/2007/12/broadcatching_itunes_a_mini_ap.html
atom_id: http://boinkor.net/archives/2007/12/broadcatching_itunes_a_mini_ap
---

Recently, I got a used Mac Mini from a friend; I decided to put it to good use as a media station in my living room, and it does indeed work pretty well (after fixing it up with codecs that are in use in the real world, but that's not the topic of this article (-:).

One thing that I find very useful is the ability to import movie files (that one might download from the vast and unfriendly-to-copyright-holders internet, as described [here](http://www.engadget.com/2004/11/23/how-to-broadcatching-using-rss-bittorrent-to-automatically/) for instance) into iTunes on my desktop mac and have the Front Row thing on the living room machine play them over the network. The steps one needs to take in order to get this working are many and tedious, and so I've created an AppleScript to do all the hard work for me. It:

* Converts an .avi movie (they usually come encoded as DivX or Xvid) into a standalone .mov file using Quicktime Player (this doesn't transcode),
* extracts the Show name, Season, Episode number, Episode name from the file name via a friend's service called "renamr" (a telnet 2.0 service, he calls it),
* imports the file into iTunes, tags it nicely, and
* cleans up after itself.

The files thus imported will pop up in the nicely remote-controllable interface, marked as "unwatched," and wait for me to finally get some spare time to sit down and watch them. (Which might very well be after the [WGA strike](http://en.wikipedia.org/wiki/2007_Writers_Guild_of_America_strike) ends, so it all should work out nicely in the end.)

If you find yourself wishing for a similar solution, download [this file](http://boinkor.net/mac/Save%20as%20TV%20show.scpt), read the comments in the beginning, skip over the MIT-style licence, and read the code until you are convinced I'm not doing anything stupid or malicious. Then, have fun using it!
