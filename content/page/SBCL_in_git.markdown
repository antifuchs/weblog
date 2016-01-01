---
layout: page
title: "SBCL in Git"
date: 2011-09-26T19:47:00Z
comments: true
sharing: true
footer: true
url: /sbcl-in-git
aliases:
- /SBCL_in_git.html
---
## SBCL CVS->Git mirror is no longer active

The SBCL cvs->git mirror repository is no longer active, as [there is now](/archives/2011/06/07/sbcl_git_repo_is_now_official/) an official [SBCL git repo](http://sourceforge.net/projects/sbcl/develop).

For historical reference, here's the old information about that repository:

## Outdated information


(If you were redirected here while looking for SBCL in Arch, my apologies. It has been [turned off](http://boinkor.net/archives/2006/10/sbcl_cvsarch_service_turned_of.html).)

This page describes the current state of the [SBCL](http://www.sbcl.org) [Git](http://git.or.cz) repository.

There is a gitweb presentation of the repostitory here: [gitweb for SBCL](http://git.boinkor.net/gitweb?p=sbcl.git).

You can clone a full copy of the SBCL repository with this command line: `git clone git://git.boinkor.net/sbcl`



## Administrative details

The repository is synchronized with upstream CVS through the Sourceforge rsync service. This means that commit propagation will be delayed by up to ~1 hour. The archive contains all commits on all branches in SBCL's past. A partially unpacked repository is 68MB in size. Typically, cloned repositories will be 47 or so MB in size.

## Some useful commands#

* To **create a branch** (off the current branch's head revision) to which you can commit, use `git branch your-branch-name HEAD`
* To **update** your tree, use `git pull`
* To **commit** to your repository, use `git commit`

All the git commands have a `--help` switch.
