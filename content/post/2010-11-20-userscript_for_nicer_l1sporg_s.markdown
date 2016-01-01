---
categories: ["Hacks", "Lisp"]
date: 2010-11-20T08:28:00Z
mt_id: 105
title: Userscript for nicer l1sp.org search
url: /archives/2010/11/userscript_for_nicer_l1sporg_s.html
---

I love the [l1sp.org](http://l1sp.org) documentation redirection service. It is quick and easy to look stuff up there if you know the name, and it has a pretty good search if you don't. However, the search results are not presented very nicely: They're very close together, and the ones I'm looking for most often (mostly CLHS pages) are buried somewhere in the middle.

So I wrote a userscript (for Firefox through greasemonkey or Google Chrome/Chromium) to improve things a bit: It enables keyboard navigation (j/k or cursor-down/up select the next/previous result, enter opens the page, and / focuses the input field), and searches for the closest match from the results (ranks them by section and then selects the shortest matching entry).

[Get the l1sp.org userscript here.](https://github.com/antifuchs/userscripts/raw/master/l1sp_org-cursor-support.user.js)

I do hope that Xach will consider adding these features to l1sp proper (maybe de-uglified a bit... that yellow highlight is really not too great). Until then, you can already enjoy the benefits of just a little javascript (-:
