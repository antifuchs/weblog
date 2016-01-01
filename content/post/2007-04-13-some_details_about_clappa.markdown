---
date: 2007-04-13T13:50:53Z
mt_id: 84
title: Some details about CLAPPA
url: /archives/2007/04/some_details_about_clappa.html
atom_id: http://boinkor.net/archives/2007/04/some_details_about_clappa
---

First of all, thanks to all who congratulated me on this, and to [Nicolas](http://www.cl-user.net/asp/zMJO6/sdataQv5utS5j$sUiDQ30nH8X8yBX8yBXnMq=/sdataQu3F$sSHnB==) for starting it all.

CLAPPA is the project I submitted for funding to the [Google Summer of Code](http://code.google.com/soc/ "Google Code - Summer of Code"), which is now being funded by [clozure](http://www.clozure.com/ "Clozure Associates - Lisp Support and Development"). The main idea behind the project is to have a package repository for [asdf-install](http://www.cliki.net/ASDF-Install "ASDF-Install on Cliki") that presents information more nicely than does [cliki](http://www.cliki.net/ "CLiki : index"). When I talked about  doing something like this project on #lisp, [Nicolas Lamirault](http://www.cl-user.net/asp/zMJO6/sdataQv5utS5j$sUiDQ30nH8X8yBX8yBXnMq=/sdataQu3F$sSHnB==) spoke up and told me he had something that was part of the way there already -- CLAPPA.

The goals of this project include checking for common maintainer errors when uploading a released package, and having a bit more machine/human-processable metadata available. This metadata will include dependencies between packages (already done in the original CLAPPA, but loads arbitrary code), versions with release dates (already done, just need all of the packages on cliki.net), and possible naming conflicts (think of the CFFI-[UFFI](http://uffi.b9.com/ "Universal Foreign Function Interface")-COMPAT system in [CFFI](http://common-lisp.net/project/cffi/ "CFFI - The Common Foreign Function Interface")). Some other sweet features will include an atom (or rss) feed for new releases, and for new releases that are dependencies of a given package, to help maintainers keep track of which updates may have broken their packages.

To draw the outline of this project a bit more clearly, here's what this project is not about:

* Dubious security features. This is not intended to be a repository of guaranteed-safe code. (Who'd guarantee we're honest?) CLAPPA is going to protect itself against malicious code, but its users must take care to protect themselves.
* Configuration management in the style of [cl-librarian](http://www.cliki.net/CL-Librarian "CLiki : CL-Librarian"). This might be done in the future, but for now the focus is on getting a useful package repository going.
* Versioned dependencies in ASDF or getting packages from DARCS. This is another interesting addition for the future. Again, the focus is on an asdf-install compatible package repository, for now.

I've set up a wiki to track development progress at [http://clappa-wiki.boinkor.net/](http://clappa-wiki.boinkor.net/) (need to be logged in to edit), and you can read the original google project submission at [this lisppaste](http://paste.lisp.org/display/39569).
