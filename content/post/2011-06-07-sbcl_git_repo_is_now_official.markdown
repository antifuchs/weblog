---
categories: ["Lisp"]
date: 2011-06-07T06:16:02Z
mt_id: 106
title: SBCL git repo is now official!
aliases:
- /archives/2011/06/sbcl_git_repo_is_now_official.html
atom_id: http://boinkor.net/archives/2011/06/sbcl_git_repo_is_now_official
---

Almost exactly four years after I [announced](http://comments.gmane.org/gmane.lisp.steel-bank.devel/7033) the availability of the first iteration of the SBCL cvs->git mirror (and a few more years after the [cvs->arch mirror even](http://permalink.gmane.org/gmane.lisp.steel-bank.devel/2536)!), we now have an official SBCL repository! Many thanks to Nikodemus for doing all the heavy lifting! (-:

I've stopped the (now defuct) cvs->git mirror. In its place, there is a new repository that mirrors the official git repo. To get the old, now static repo, see [sbcl-cvs-import](http://git.boinkor.net/gitweb/sbcl-cvs-import.git).

If you are a committer and have any old checkouts around, please make sure that you follow [the procedure for eliminating unnecessary branches / tags](http://thread.gmane.org/gmane.lisp.steel-bank.devel/15983/focus=15987).
