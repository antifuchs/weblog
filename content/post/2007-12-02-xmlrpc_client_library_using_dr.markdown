---
date: 2007-12-02T16:30:37Z
mt_id: 89
title: XML-RPC client library using Drakma and CXML
aliases:
- /archives/2007/12/xmlrpc_client_library_using_dr.html
atom_id: http://boinkor.net/archives/2007/12/xmlrpc_client_library_using_dr
---

A few weeks ago, I tried using [S-XML-RPC](http://common-lisp.net/project/s-xml-rpc/ "S-XML-RPC") for use with a hunchentoot-based web interface to [rtorrent](http://libtorrent.rakshasa.no/ "The libTorrent and rTorrent Project - Trac"). Unfortunately, it comes with a long list of dependencies that are already implemented better by [Ediware](http://weitz.de) such as [drakma](http://www.weitz.de/drakma/ "DRAKMA - A Common Lisp web client"): it can speak HTTPS, connect via proxies, and allows cookies (although I'm not aware of any xml-rpc implementation that supports this (-:).

So I re-implemented s-xml-rpc's client part to use the libraries that are already available in [clbuild](http://common-lisp.net/project/clbuild/), and got a pretty pleasant-to-use library. To simply invoke an xml-rpc method, use:

``` cl
* (xrpc:call "http://betty.userland.com/RPC2" "examples.getStateName" '(:integer 41))
"South Dakota"

* (xrpc:call "http://time.xmlrpc.com/RPC2" "currentTime.getCurrentTime" ())
#<CXML-RPC::XML-RPC-DATE 20071202T06:56:43>
```

It's entirely undocumented right now, and some names (and interfaces) may be subject to change, but I'm making it available now anyway, at [my git repository](http://sbcl.boinkor.net/gitweb?p=cxml-rpc.git;a=summary).

The plans for the near future (e.g. next weekend) include a server part based on [Hunchentoot](http://weitz.de/hunchentoot), and, um, test cases and documentation. Stay tuned!
