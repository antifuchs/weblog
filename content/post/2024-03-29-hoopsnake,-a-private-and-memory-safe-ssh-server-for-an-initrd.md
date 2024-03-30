---
title: "Hoopsnake, a Private and Memory Safe Ssh Server for an Initrd"
date: 2024-03-29T17:52:18-04:00
draft: true

categories: ["Hacks", "Security", ""]
---
Over the past few years, all of the Linux machines I run (and I only run them headlessly) have been running with full-disk encrypted storage; that's just good practice, but it's kind of annoying: If they can't boot, worst case I have to get a monitor and keyboard plugged in and type an inconvenient password. Over time, all the systems I set up trended towards the following setup, which has sat just about at the sweet spot of giving me a safe feeling and being not-too-inconvenient:

* [dropbear ssh](https://matt.ucc.asn.au/dropbear/dropbear.html) and [busybox](https://busybox.net) in the pre-boot environment (aka initrd) to provide a minimal shell access, and
* [tailscaled](https://tailscale.com) with a truly [*wild* set of patched software](https://gist.github.com/antifuchs/e30d58a64988907f282c82231dde2cbc) to give me private access so dropbear doesn't listen on the public internet,

I didn't much love this: There's a bunch of very custom network configuration that has to happen, tailscaled requires statically-linked iptables and a writable state directory (or API keys checked in), and the whole initrd by default has you add secret material in an unencrypted way. Is there a better way? Now, there is (I think)!

<!--more-->

# State of the art: tailscaled + dropbear ssh

It took me a while (and a bunch of sanity points) to come up with [the nix config in that gist](https://gist.github.com/antifuchs/e30d58a64988907f282c82231dde2cbc), and I was always somewhat worried that tailscaled would eventually require the ability to store actually-persisted changes in its state (which this configuration doesn't allow for - it just passes a prepared state file in).

What's more, dropbear always felt like a difficult pill to swallow for me: It's yet another SSH daemon, written in C, that wants to listen on the public internet, and it runs in an environment that usually has no effective auditing or logging. So if it gets broken into, you are none the wiser!

And even worse, the API credentials and SSH key material used by all these initrd tools sits unencrypted on disk, world-readable for any service to just pick up.

Few of these things are complete dealbreakers, but they all add up to a fair bit of difficulty to get a system into a state where your trust in the data integrity and privacy provided by it can be described as anything other than "unmoored".

There are ways to get things tightened down: [Will Fancher](https://elvishjerricco.github.io/) published the [configuration necessary](https://github.com/ElvishJerricco/stage1-tpm-tailscale) to get all this working with secrets & writable tailscale state stored in a disk partition that gets automatically decrypted with a secret sealed by TPM2 registers (which, together with [nixos secure boot](https://github.com/nix-community/lanzaboote)); it's still a lot of custom work that relies on the system layout & configuration working together _just so_... otherwise your system either won't boot, or you expose it to dangers again.

# A more ... modern(?) approach

When we work on security-relevant things, we should ensure that things are resilient and are secure by default. If only we could get a tailscale client working that doesn't require a wide variety of networking tools to be installed in the surrounding system. [Oh wait](/2023/07/tsnsrv-or-easily-accessing-services-on-your-tailscale-network/), I already know how to do that. And if only there was a decent OpenSSH client out there that was written in go. [Oh](https://github.com/gliderlabs/ssh) [WAIT](https://pkg.go.dev/golang.org/x/crypto/ssh), an SSH server ships with go, and there's a really nice wrapper library too!

And if only there was a way to make the whole thing stateless, so it doesn't need anything other than a private key and whatever long-lived API secrets tailscale needs to keep my clients working, oh wait!!!, you can store [tailscale oauth clients](https://login.tailscale.com/admin/settings/oauth) in encrypted [systemd credentials](https://www.freedesktop.org/software/systemd/man/latest/systemd-creds.html) and suddenly all your secrets can only be decrypted by the machine you intend to decrypt them on. Hence:

# Introducing `hoopsnake`

[Hoopsnake](https://github.com/boinkor-net/hoopsnake) is an SSH server that listens only on your tailnet. It's entirely written in native go, and compiles into a static binary. It comes with a nixos module that makes deploying it a pretty drop-in operation, and exposes a prometheus endpoint (optional) too, so you can define alert rules for whenever a machine you need to unlock sits at a shell prompt for longer.

Its overall shape is strongly influenced by that of [`tsnsrv`](/2023/07/tsnsrv-or-easily-accessing-services-on-your-tailscale-network/), but it is a separate program just because the needs of an HTTP proxy and an SSH server are so different.

## One really good way to deploy it: systemd in stage1, with a TPM2

I mentioned earlier that we want to make secure and resilient the default; that's a great goal, but the current state of the world doesn't make this too easy yet. A bunch of things are being reworked to get us closer to these defaults, and a bunch of changes are [in flight *right now*](https://lwn.net/SubscriberLink/965631/bcc60b196d158d9e/#:~:text=the%20multi%2Dyear%20effort%20to%20upgrade%20NixOS%27s%20init%20system): good support for secureboot; support for really supervising the processes in an initrd environment; support for hardware-encrypted secrets.

One day we'll have nice things and it'll be inconceivable that we had to just shrug and accept either the burden of a massive custom-built system, or plaintext secrets just sitting around, or systems that need crash carts every time you upgrade their kernel.

(If I sound bitter, sorry, I'm really not - just very excited for the future! It's a stark contrast to the present.)

## PS: Not a reaction to the xz security incident

Please don't think I planned on publishing this software right in the middle of an incident involving [a horrifying supply-chain/long-game attack on a compression library targeting `sshd` servers](https://blog.qualys.com/vulnerabilities-threat-research/2024/03/29/xz-utils-sshd-backdoor#technical-details-of-cve-2024-3094). My desire for a remote shell access system that's not written in C way predates this incident, but also it's not clear that being written in go would have in any way prevented this exact same situation if an attacker chose to target a slightly different part of the ecosystem (and it's not like dropbear was targeted by the attack anyway).

However, I think it makes me pretty hopeful every time the variety of choice expands when it comes to software we run!
