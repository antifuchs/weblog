---
date: 2007-04-06T07:56:06Z
mt_id: 81
title: boinkmarks outage...
url: /2007/04/boinkmarks-outage/
atom_id: http://boinkor.net/archives/2007/04/boinkmarks_outage
---

Still in Cambridge, "sharing" connectivity on a wireless access point kindly provided by security-unaware residents, so just a short update on the boinkmarks thing: something on this machine is making the linux OOM killer go mad (both in linux 2.6.16 and 2.6.18, running on amd64). It will kill perfectly well-behaved processes although there seem to always be hundreds of megabytes of real RAM (not to mention swap) available. Then, some time later, it will start complaining on the console about running out of memory while oomkilling, in an endless loop.

Thank god this machine has a remote admin interface. I'll try to fix the real problem when I am back home.
