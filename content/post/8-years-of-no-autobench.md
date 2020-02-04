---
title: "Somewhere between 8 and 11 years without boinkmarks"
date: 2020-02-02T14:22:01-05:00
Categories: [Lisp]
Tags: [Retrospective, "Things that are missing"]
---

About 8 years ago, I turned off and carted away the server running [boinkmarks](https://boinkor.net/2005/02/autobench-released-at-last/) (aka [`autobench`](https://github.com/antifuchs/autobench)), the benchmarking and performance tracker for [SBCL](http://www.sbcl.org/). It seems like its [last planet.sbcl.org](http://planet.sbcl.org/2009/4.html) entry was around 2009-04-11.

At the time, it had run benchmarks on every revision[^revisions] of SBCL since 2004 - I'd been running it for 8 years, as long as I now have not run it.

Time for a retrospective!

<!--more-->

In the 8 years (2004 - 2012) that it did run, I feel that it provided a pretty useful service to the community. In a world where [Travis CI](https://travis-ci.org) was [6 years away the day it was announced](https://www.crunchbase.com/organization/travis-ci), boinkmarks were effectively a CI service for this one, very specific CL compiler.

## What did it do well?

Autobench did a whole bunch of things, for every "commit" -- then, as imported in to the git repo from CVS -- building [the manual](https://boinkor.net/2004/06/sbcl-manual-now-auto-built/), checking that the compiler itself be built with at least one older version of itself, and running its performance suite.

Autobench then stored those benchmark results in a postgresql database and provided a web frontend that visualized the history of each test.

There was also an RSS feed that computed the "significant" changes between one revision and the previous one - the intent was to make performance regressions easy to spot.

Autobench alerted me to build failures via email - usually, that was due to an environmental condition (it ran on my personal "server", and that was before [configuration as code](https://blog.nelhage.com/post/declarative-configuration-management/)), but when anything pointed to a build failure in the source code, I either tried to fix it myself, or alerted the other committers.

Quite importantly to me, autobench didn't operate on a "daily" or other timeslotted granularity, where it would build each day's `HEAD` revision. It built every commit, and it benchmarked every commit. That made it much easier to figure out which commit broke a build or caused a performance regression.

Over time, SBCL-the-compiler (which compiled SBCL-the-codebase into SBCL-the-compiler) got much faster - both at compiling itself (12-20 minutes when I started, 7 minutes when the machine got wrapped up) as well as in the code it generated. I don't know if that is directly attributable to the performance regression suite (see below), but I'd like to think it did highlight regressions that the suite tested for!

## What was not so great?

Despite all my attempts at making it generic, autobench was extremely specific to the target -- SBCL -- and the benchmark suite, [`CL-BENCH`](https://github.com/antifuchs/autobench/tree/master/cl-bench) (so much so that I included a copy of that library in autobench's source code itself). Forget about using it for tracking performance of anything that wasn't a CL compiler... or a CL compiler that could be bootstrapped by running a simple command-line invocation.

While I eventually got it to build CLISP (another Common Lisp implementation), the automatic builds for it never really reached the point where it could really build unsupervised, let alone finish running its benchmarks[^git-as-the-norm].

Autobench (and CL-BENCH) had very little idea of how to "do" benchmarks in a way that's statistically meaningful. You can see some background on that in [this delightful paper](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.59.7380):
* The benchmarks themselves mostly tested changes in garbage collector speed, or method call performance.
* While I did my best to "echo scientific convention" -- running them 3 times & working with standard deviations -- it took several years until I finally started changing the iteration times on each benchmark, so they ran for several seconds each... eliminating a whole ton of noise.
* While the machine that the benchmarks ran on didn't have a ton to do, it certainly wasn't he quietest neighbor to the benchmarking task itself. These days, I would probably try to again run on "bare metal", but also eliminate things like `cron` and other shared-tenancy systems.

## What has changed since autobench got turned off?

Git and other "meaningful changeset" version control systems are the norm now. It's hard to imagine from today's perspective, but many projects that were around for a long time in 2010 were still using (or had just stopped using) CVS. Nowadays, you can bisect history! Or look at the "previous commit" and compare different points in history, along semantic boundaries. Pure luxury!

CI is the norm for most open-source projects now. Thanks to [travis](https://travis-ci.org) and all the [many](https://circle-ci.com) [other](https://gitlab.org) [CI](https://azure.microsoft.com/en-us/services/devops/pipelines/) systems out there that are free to OSS projects, we've normalized the idea that software should (a) compile and (b) run its tests! That's pretty huge.

The [pull request](https://help.github.com/en/github/collaborating-with-issues-and-pull-requests/about-pull-requests) type workflow is now mainstream. Automation runs regression tests on a proposed change and will sometimes even block it from going into the main codebase if a test should fail.

Test coverage seems to be, if not the norm, then a stretch goal for projects now, too. Since [codecov](https://codecov.io) is free for OSS projects too, there's several that put a badge on their homepage.

There now exist very high-quality, mainstream, libraries for measuring performance! The [criterion crate](https://bheisler.github.io/criterion.rs/book/index.html) is one extremely good example - it'll automatically find the number of iterations that a benchmark needs to run, perform quite sophisticated statistical analysis and offers some great ways of comparing the current state to previous baselines.

## Thoughts

From [Nelson's incredibly good post on why Sorbet is fast](https://blog.nelhage.com/post/why-sorbet-is-fast/), I take one thing away: Software starts at one speed and it stays fast by avoiding performance regressions. (And sometimes, it even gets faster!)

The default way a developer interacts with software they work on is by making it slower: New features make programs more complex and usually consume more time to execute and render, processing more data takes more time, bugs creep in that don't affect the correctness but do affect performance. We just don't notice when we make things worse by making them slower.

Right now, it feels like software's performance is still treated as an externality to development, like "can build" and "can run tests" was, 15 years ago. There is little in the way of automated systems in place (in OSS, at least!) that track or even alert on performance repressions. Projects mostly rely on users to feel pain & to report if performance gets worse. And when performance gets worse, it's hard to track down where the slowdown happened.

Even Sorbet's [main method of performance regression testing](https://blog.nelhage.com/post/why-sorbet-is-fast/#continuous-attention-to-performance) was to run the test suite on something big and rely on its developers to notice when tests took too long!

Projects do exist that track performance history:

- [rust](https://perf.rust-lang.org/),
- [chromium](https://chromium.googlesource.com/chromium/src/+/master/docs/speed/perf_regression_sheriffing.md),
- [PyPy](https://speed.pypy.org/),
- [Firefox](https://arewefastyet.com/win10/overview?numDays=60)
- and some others.

These are rather the exception than a normal thing to do on a project, though: It’s not a coincidence that these are all pretty large projects with a decently large volunteer pool. Benchmarking (especially benchmarking something that changes rapidly in a reasonably stable environment) takes *work*. And analyzing that performance data, too, takes work.

This [rOpenSci issue](https://github.com/ropensci/unconf17/issues/56) puts it quite succinctly: Few people have an answer to the equation “codecov.io is to code coverage as ??? is to benchmarking”. There's very few mainstream tools I know of that will take benchmark data and perform the kind of rudimentary analysis autobench did. Asking around, two systems stand out: python’s [airspeed velocity](https://asv.readthedocs.io/en/stable/) (which has a pretty cool [demo](https://pv.github.io/numpy-bench/) site) and [codespeed](https://github.com/tobami/codespeed). I haven’t tried either yet; at least asv seems to cover autobench’s feature set, and the demos look good! But not everyone has the time/patience/digitalocean credits to host a python webapp somewhere... the way you get a concept to the people is by running it "as a service," and free for OSS projects.

What I wish would happen is that somebody[^that-isnt-me] built&productized that "codecov for performance data"  service. Forget about the actually-running-the-benchmarks problem for now[^running]: Ingest a reasonable format for benchmark data into a database, host a mostly-ok looking UI around it and hook into github's commit/pull request check workflow. If you built this --- I think people would love it.

And, I think best of all, you'd help build software that is faster, whose maintainers are confident that it's not getting worse.


[^revisions]: At the time, SBCL's canonical source code repository was sourceforge's CVS server. "Revisions" corresponded to an increment of the version number in `/version.lisp-expr`, using the same commit message for that file and the rest of the code, mentioning that version number. If you don't know what that means, count yourself lucky! You escaped CVS and live in the world of meaningful history now!

[^git-as-the-norm]: The other problem in getting something with less "commit discipline" than SBCL built was that I had to run a CVS->git gateway. I'm definitely blaming the grey hairs I found when I turned 25 on that gateway.

[^that-isnt-me]: I wish I had the time to build this. But, alas! Fulltime employment!

[^running]: People who care enough about consistent results will need to run their own hardware; others, doing casual analysis for their OSS projects (like me!) can run their benchmarks on a free CI host and learn to deal with variance in the milliseconds, I guess.
