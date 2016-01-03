---
Categories: ["Hacks"]
Description: ""
Tags: []
date: 2016-01-02T20:24:26+01:00
title: Hosting my blog on Google App Engine with Letsencrypt
---

Editing my [last post](2015/12/2015-in-books/) in
[Octopress](http://octopress.org/) was such a pain that I decided to
switch the blog over to [Hugo](http://gohugo.io). While doing that, I
decided that the yak stack wasn't deep enough and that I should be
moving my blog to https in the process. Here is my story (and links to
automation shell scripts!)

<!--more-->

(This is what happens when you give me a pot of black tea on New
Year's Day after 6 hours of sleep!)

## The Yaks

I was hosting this blog on [Amazon S3](https://aws.amazon.com/s3/) -
it's static files, so that seemed reasonable. However, you can only
host non-https sites on S3 - to get https, you have to use
[Cloudfront](https://aws.amazon.com/cloudfront/), and then *that*
would require that cloudfront talks to S3 over http - that's pretty
ridiculous.

My colleague Carl found a great solution, though: If you write
[a tiny amount of configuration](https://github.com/antifuchs/weblog/blob/master/app.yaml),
and a
[go file containing `package dummy`](https://github.com/antifuchs/weblog/blob/master/dummy.go),
you can get [Google App Engine (GAE)](https://cloud.google.com/) to host your weblog's static files on
*their* infra, with a reasonable HTTPS story!

All that you need now is an SSL certificate, and hey -
[letsencrypt](http://letsencrypt.org/) gives you free certificates
with reasonable (and most importantly, automatable) processes - perfect!

## Getting that SSL Certificate

The
[default letsencrypt client](https://github.com/letsencrypt/letsencrypt/)
expects to run on your web server as root. Google app engine however
doesn't give you any of that - you get no web server, no code exec and
most certainly no root.

This sounds displeasingly impossible, but thankfully, we don't have to
use the letsencrypt client, except to set up an account. Once I had
the private key file, I used
[letsencrypt.sh](https://github.com/lukas2511/letsencrypt.sh) by Lukas
Schauer to automate the SSL certificate issuance process.

### Background

This is how letsencrypt operates (they have a
[really really good technical document too](https://letsencrypt.org/howitworks/technology/),
so feel free to skip this section): They first check that you have
access to the domain that you request the certificate for, by
providing you a challenge URL and response body that they expect to
get back when they hit that URL. Once they can see the right response
(with a timeout), they issue a certificate for your private key.

### The Automation Caper

With google app engine, we can deploy web apps, so I initially wrote
[a little go program that would respond to these requests](https://github.com/antifuchs/weblog/blob/427d5c5141bfa83d024930bd18b817c7647a5196/letsencrypt.go)
and kept it under source control. This wasn't great for a number of
reasons, and the biggest one was that I had to copy/paste these tokens
back and forth - a toilsome process.

Now, letsencrypt.sh has a "hook" facility for the certificate issuance
process: It calls a shell script or function for every step of the
challenge/response flow. Writing the script to do the right thing was
pretty trivial, and this is what it does (follow the links if you like
bash scripts):

* When letsencrypt requests a response to a challenge, the hook
  [now writes this go handler and deploys it](https://github.com/antifuchs/weblog/blob/ff6d95f/scripts/letsencrypt-hook#L12-L42),
* when the challenge/response process is done,
  [it cleans up that handler](https://github.com/antifuchs/weblog/blob/ff6d95f/scripts/letsencrypt-hook#L44-L46), and
* when the certificate is issued, it
  [provides instructions](https://github.com/antifuchs/weblog/blob/ff6d95f/scripts/letsencrypt-hook#L48-L59)
  to correctly upload the SSL certificate to GAE pages (I couldn't
  find an API to do this, so it uses `pbcopy` to put the relevant
  things on the clipboard - please
  [let me know](https://twitter.com/antifuchs) if you find one!).

All this is held together by a kinda convoluted Makefile - here are
the most important targets:

* `make deploy` calls
  [this script](https://github.com/antifuchs/weblog/blob/ff6d95f/scripts/build)
  to generate the latest HTML, and deploy the app to GAE.
* [`make certificates`](https://github.com/antifuchs/weblog/blob/ff6d95f/Makefile#L17-L18)
  calls `letsencrypt.sh` with the right arguments and should allow me
  to renew the certificates that I created once they are closer to
  expiring (2016-03-31!)

### Annoying Things That Cost Me Way Too Much Time

Two things in this setup were really pretty frustrating:

One, letsencrypt.sh requires a perl program to extract your regular
letsencrypt client's private key into usable format (they store its
RSA parameters in JSON, everything else under the sun expects the key
format to be PEM).

This perl program requires
[Crypt::OpenSSL::Bignum](http://search.cpan.org/dist/Crypt-OpenSSL-Bignum/)
and [::RSA](http://search.cpan.org/dist/Crypt-OpenSSL-RSA/), which
were serious pains to install under El Capitan. What I ended up doing
was install
[openssl from homebrew](https://github.com/Homebrew/homebrew/blob/master/Library/Formula/openssl.rb)
and link the headers (which they place out of the way) into place so
that the install process could find them, like so:

`ln -sf /usr/local/opt/openssl/include/openssl/ /usr/local/include/openssl`

With the symlink in place, these two modules could install, and I
could finally convert the private key to the right format. (Finding
the right combination of cpan and file system things took me about an
hour, ugh.)

Conclusion: letsencrypt, your client's private key format sucks &
converting it into anything remotely useful is annoyingly difficult.

The second frustrating/unfamiliar thing that cost me time was that if
you have two GAE apps (one for a live blog and one for a "test" blog)
and a certificate that covers both blogs' domains, you have to upload
the same certificate to both apps so that the GAE custom domain picker
can even refer to it.

Conclusion: The GAE SSL cert upload form is convoluted and annoying,
and I really want an API for this.

## How well does it work?

I could bring my blog up under SSL within less than 4h, and that
included a bunch of hacking. If you use the automation scripts and
tricks for avoiding pitfalls I mentioned above, you should be able to
get this running in far less time (I hope)![^1]

My [weblog's git repo is here](https://github.com/antifuchs/weblog).
If you do use this, please let me know how it goes!

[^1]: I'll probably write an update full of screams of frustration if cert renewal time comes and everything fails.[^2]

[^2]: ...but you won't be able to read that update because my blog's SSL config will be broken. So it goes! (-:
