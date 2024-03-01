---
title: "Saved by NixOS Integration Tests, Surprisingly"
date: 2024-02-29T08:55:32-05:00

categories: ["Hacks", "Nix"]
---
Recently, [tsnsrv] has been getting a lot of [high-quality
contributions](https://github.com/boinkor-net/tsnsrv/pulls?q=is:pr+-author:antifuchs+-label:dependencies+state:merged)
that add better support for Headscale and custom certificates, among
other things. As they always do when things change, [bugs crept
in](https://github.com/boinkor-net/tsnsrv/issues/112), and
frustratingly, not in a way that existing tests could have caught:
Instead of the go code (which has mildly decent test coverage), it was
bugs in the nixos module!

This was a great opportunity to investigate if we can test the tsnsrv
NixOS module, and maybe improve the baseline quality of the codebase
as a whole.

A recent blog post series on the NixOS test driver
([part1][nixos-tests-pt1], [part2][nixos-tests-pt2]) made the rounds
showing off what it can do, so this felt like a tractable project to
take on. Here's my experience with it.

<!--more-->

# How do NixOS integration tests work?

If you haven't read the blog post series above, here's a quick
summary: Your nix flake has an attribute set called `checks` that each
define a test. Each test has definition for a set of NixOS machines
(built as VMs) and a test script written in typechecked & linted
python. Each machine exists as a variable in that python script, and
you tell it things like `wait_for_unit` (wait until a systemd unit is
started) or `get_screen_text` (OCR the pixels on the VM's screen).

To a jaded programmer, this sounds like the typical e2e/high-level
integration test disaster: The environment in which you run the tests
can cause variances in how programs interact, leading to such
delightful test failures as "Waiting for a thing timed out" (more on
that later). But.

# Why do they work?

To say I was surprised that, huh, they do in fact not break randomly
all that much might be an understatement. I think I have an idea on
the factors that make it work so well:

* The machines constructed by the test framework are extremely
  realistic, running real NixOS. It's not hard at all to define
  one. [Look at headscale's integration
  test](https://github.com/NixOS/nixpkgs/blob/master/nixos/tests/headscale.nix#L20-L26) -
  that defines a working machine running `tailscaled`.

* The network connecting these machines is extremely realistic. Your
  machines can see each other and they have DNS entries. They have
  firewall rules that are effective, just like in the real world.

* The test driver itself makes use of a ton of qemu's features. You
  can OCR text(!), you can simulate plugging in a USB drive. You can
  reboot the machine, if you want!

* _And despite all that realism_, the default setting for software on
  the machine is "everything keeps running". NixOS comes with
  extremely reasonable defaults and has a really well-done integration
  with systemd. And if you need to deviate from these defaults, you
  can!

* But I suspect most of all, the nixos integration test suite gets
  exercised on a lot of [pull requests to the nixpkgs
  repo](https://github.com/NixOS/nixpkgs/pulls?q=is:pr+is:closed), of
  which there have been nearly a quarter million merged. That tends to
  take the sharp edges off.

A working test takes only about 1 minute to complete. I was very
surprised at that speed.

# Getting it working for tsnsrv

So tsnsrv's nix integration is via a [nix
flake](https://zero-to-nix.com/concepts/flakes), which is a relatively
new concept in nix; it uses [flake.parts](https://flake.parts), which
is a relatively new library for working with flakes; all that
contributes to stuff just requiring a lot of trial and error. I did
this to myself, you could say.

Fortunately, it's not super difficult to come up with the buttons to
push to get a running test, but it did take some digging. Here's the
[`flake-part.nix`](https://github.com/boinkor-net/tsnsrv/blob/main/nixos/tests/flake-part.nix)
file that will invoke a test. Annoying bits to figure out included:

* What is the place defining the `runTest` function that defines a
  test? [Even experienced nix contributors get confused about
  it](https://github.com/boinkor-net/tsnsrv/pull/115#discussion_r1503914212). [Answer](https://github.com/boinkor-net/tsnsrv/blob/main/nixos/tests/flake-part.nix#L19).

* By default, all checks in flake.parts are defined for all kinds of
  system (linux and mac); now `nix flake check` fails on my
  mac. Answer: [Only define it on linux
  systems](https://github.com/boinkor-net/tsnsrv/blob/main/nixos/tests/flake-part.nix#L21).

And at that point, I had a working integration test that finished in
under 50s on my linux machine, and I could iterate.

So then, it was just a few-hours affair to come up with a test that could:
1. set up headscale
2. set up tailscale that authenticates to headscale
3. set up tsnsrv that authenticates to headscale and sets up a proxy
   listening for plaintext HTTP.
4. Try to reach a site exposed on tsnsrv

...and that worked: tests passed (and I had tried breaking a few
things to see the tests fail - which they did!), so I merged the
change that introduced that e2e test.

## But oh no: Surprise timeouts.

With this in place, I was confident to proceed merging some more of
the great contributions that people made, and the first one I
attempted to merge immediately failed that e2e test: The tests said
"timed out". That was two hours after I'd merged the tests, so I was
definitely suspicious of the tests themselves: end-to-end tests only
do this if they're very distressed, etc etc.

Reading the test log properly though revealed one line that stood out:
`tsnet: you must enable HTTPS in the admin panel to proceed` - tsnsrv
sets up a plaintext HTTP proxy! Why would I need to open the admin
console? Oh, right, because the pull request I intended to merge
changed the logic around plaintext HTTP listeners, and it had a logic
bug! (In case you're wondering, [it was a `||` / `&&` operator
precedence
bug](https://github.com/boinkor-net/tsnsrv/compare/218c46a60972376038a0691a4302f92a0e1f198c..f83b6a279bbf4917aa1c060fbd8dc84b151de75b#diff-8d9ca23280f24fe6444d03ae46e7a15dd152170f32f57f978dfbdfd3cfe8ff55L301).)

That timeout was a legitimate inability for tsnsrv to come up
properly.

So, within 2 hours of setting up end-to-end tests, they had caught a
legitimate regression.

# In Summary: I'm a fan.

I've only been using it for a few days now, but as of now I'm a huge
fan of the NixOS integration testing library. It's definitely the most
pleasant (if still a bit idiosyncratic) end-to-end testing framework
I've used so far. It seems to be pretty easy to control for
timing/race condition errors and best of all, it's already caught a
bug that I would not have found for months.


[tsnsrv]: https://github.com/boinkor-net/tsnsrv
[nixos-tests-pt1]: https://nixcademy.com/2023/10/24/nixos-integration-tests/
[nixos-tests-pt2]: https://nixcademy.com/2023/12/01/nixos-integration-tests-part-2/
