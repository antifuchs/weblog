---
categories: ["Lisp"]
date: 2006-11-03T11:13:41Z
mt_id: 50
title: Using git bisect to locate bugs in SBCL
url: /archives/2006/11/using_git_bisect_to_locate_bug_1.html
atom_id: /archives/2006/11/using_git_bisect_to_locate_bug_1
---

I've announced the git repository a few weeks ago. Here's something very nice you can do with it: Run a binary search on revisions to find out the version of SBCL that caused a bug. This helps enormously when searching for the cause of bugs.

You can use the CVS repository for this, too, but it doesn't include any of the nice functions I mention in this tutorial; you need a start time and an end time, and do month/day/hour/minute calculations yourself. Git allows you to bisect based on changesets, and it includes a nice graphical representation of bisection progress.

Here's how you do it:

1. Make an automated test case that you can run from a script. This may seem optional, but it gets tiresome (and error-prone) if you test manually.

2. Check out the sbcl tree: `git clone git://sbcl.boinkor.net/sbcl`

3. Change to the directory, look at the revision log: `git log`

  The lines starting with "commit" contain sha-1 commit IDs. A few lines after that, you see the log summary containing the sbcl revision number.

4. Find the version that you know is broken, and a version that you know works and copy their commit IDs. I'll call the good version's commit ID <acronym title="The good revision">`6584a2c88efaa6931083721adae2f9f10e0fefd5`</acronym> and the bad one's commit ID <acronym title="The bad revision">`1de12891f900d156ed035a097561ecd7755a256a`</acronym>.

5. start bisection search: `git bisect start`

6. Now mark the good and bad revision:
 * `git bisect good ` <acronym title="The good revision"> ` 6584a2c88efaa6931083721adae2f9f10e0fefd5`</acronym>
 * `git bisect bad ` <acronym title="The bad revision"> ` 1de12891f900d156ed035a097561ecd7755a256a`</acronym>

  This will give you output similar to:
  <pre>Bisecting:        9 revisions left to test after this
[90a83478829f33b91f6300c183b374a968bc13c6] 0.9.18.20: correct step-frame logic on non-x86oids</pre>

  From now on, after every step, you can look at a first bisection history in gitk: `git bisect visualize`

7. Start the actual bisection: `git bisect next`

  This gives you:
  <pre>Bisecting:        9 revisions left to test after this
[90a83478829f33b91f6300c183b374a968bc13c6] 0.9.18.20: correct step-frame logic on non-x86oids
$ cat version.lisp-expr
[...]
"0.9.18.20"</pre>

8. Run the test script.

 * If it fails, use `git bisect bad` to mark the current revision as bad,
 * if it succeeds, use `git bisect good` to mark it as good.

  You'll get something like:

  <pre>Bisecting:        5 revisions left to test after this
[7bad074650949dc5427711b93ff615d9c17308d9] 0.9.18.25:</pre>

  As mentioned above, you can always view the bisection history using `git bisect visualize`.
9. Repeat steps 7 and 8, until there are no more versions to bisect. After running the final test and marking the version with `git bisect good` or `git bisect bad`, You'll get something like the following output:

```
a30a3d82293eca3eb036ea0c713b0da31c0467dc is first bad commit
commit a30a3d82293eca3eb036ea0c713b0da31c0467dc
Author: Nikodemus Siivola &lt;nikodemus@random-state.net&gt;
Date:   Thu Nov 2 15:11:25 2006 +0000
    0.9.18.27: fix darwin build
     [...]
```

And that's it! You identified the broken revision. When reporting bugs to the sbcl mailing list, please include the version number (in the example above, that would be 0.9.18.27).


If you want to cancel or pause bisection at any time, you can use the following commands:

* `git bisect log > bisection-logfile` # (optionally) save the current progress
* `git bisect reset` # reset bisection to the revision where you started

To resume bisection from a log file, use `git bisect replay bisection-logfile`.

I hope this guide helps you identify bugs quickly. Good hunting!
