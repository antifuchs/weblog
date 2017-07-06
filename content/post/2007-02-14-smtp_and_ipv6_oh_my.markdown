---
date: 2007-02-14T15:46:42Z
mt_id: 75
title: SMTP and IPv6, oh my!
atom_id: http://boinkor.net/archives/2007/02/smtp_and_ipv6_oh_my
---

This entry is a reminder to myself more than a contribution to any [ongoing or dead discussions](http://www.ripe.net/ripe/maillists/archives/ipv6-wg/2005/msg00526.html) about this topic. (Thanks to **Stoffi** for the initial explanation.)

baker (the machine that's acting as a mail and web server) has had an IPv6 address and AAAA entry for a few weeks now. Unfortunately, some E-Mail servers don't handle this well. They see this in my zone:

    @		IN	MX   10 baker.boinkor.net
    baker	IN	AAAA [ipv6 address]
    baker	IN	A    [ipv4 address]

and go "I know! I'll just connect to the ipv6 address!" Unfortunately, not every host is on a part of the net where they can connect to globally-visible IPv6 addresses yet, so that connection fails. Instead of connecting to the fallback IPv4 address, they'll consider that MX entry broken and fail to deliver mail to me. Despite the volume of spam I get these days, this isn't what I want.

Indeed, backup MXes are the solution. Here's a setup that allows the poor broken MTAs to still connect to my mail host:

    @			IN	MX   10 baker-v6.boinkor.net
    @			IN	MX   20 baker-v4.boinkor.net
    baker-v6	IN	AAAA [ipv6 address]
    baker-v6	IN	A    [ipv4 address]
    baker-v4	IN	A    [ipv4 address]

So, if an affected MTA comes in and fails to deliver to the primary (ipv6-enabled) mail host, they can always connect to the secondary mail exchanger with IPv4.
