---
layout: page
title: "SBCL in Arch"
date: 2011-09-26T19:58:00Z
comments: true
sharing: true
footer: true
url: /sbcl-in-arch
aliases:
- /SBCL-in-arch.html
---
<p><strong>SBCL cvs-&gt;arch synchronisation is turned off. The last
version is 0.9.17.17.</strong> You can use the <a href="http://sourceforge.net/projects/sbcl/develop"><strong>official</strong> SBCL git repository</a> now! I
don't update this page any more. See <a href="/archives/2006/10/sbcl_cvsarch_service_turned_of.html">This blog entry</a>
for details and updates.</p>


<h2>Introduction and Health Warnings</h2>

<p> This was a (pretty stable) beta version of what an Arch archive of
the current SBCL CVS repository would look like. The archive is
<strong>no longer</strong> synched with the SF.net developer's
CVS.</p>

<h3>Warnings (do no longer apply)</h3>

<ul>
<li>The CVS sync did not really happen every hour. Sometimes, there
were ssh key exchange troubles, which prevented a successful sync from
happening. If you wanted to check out something that went into devel
CVS an hour (or two) ago, but it wasn't present in the arch archive,
chances were that there were ssh key exchange troubles.</li>
<li>The CVS sync also stopped when it detected a possible file move
(i.e. a remove and an add in the same commit) and required manual
intervention from me. This didn't happen very often, though.</li>
</ul>

<h2>Checking things out from the archive</h2>

<p>If you can't tell from the past tense and the note at the top:
<strong>The CVS-&gt;arch synchronisation service is turned
off.</strong> Use git. If you still want the arch archive, do
this:</p>

<ol>
<li>Get <a href="http://bazaar-vcs.org/Bazaar1x/Downloads">baz (bazaar version 1)</a> and install it.</li>
<li>Run
<pre class="example">baz register-archive http://sbcl.boinkor.net/sbcl@boinkor.net--2005/
</pre></li>
<li>Then, check out a revision into the directory "sbcl--main" (you can change the destination directory, of course). I'll assume that you want to get the latest revision in the 0.9 (current) branch:
<pre class="example">baz get sbcl@boinkor.net--2005/sbcl--main--0.9 sbcl--main
</pre></li>
</ol>

<p>For further information about arch, see <a href="http://wiki.gnuarch.org/moin.cgi/Learning_20Arch_20commands_20for_20CVS_20users">the Arch introduction for CVS</a> users and <a href="http://regexps.srparish.net/tutorial-tla/arch.html">the Arch tutorial</a>.</p>

<h2>Statistics concerning the archive</h2>

<p class="first">When I turned syncing off, SBCL, as hosted on <a href="http://sf.net/projects/sbcl">http://sf.net/projects/sbcl</a>,
consisted of 3176 revisions. The arch archive's size is 190MB
(containing only the MAIN branch). A greedy, non-sparse revision
library for the entire archive was ~2.5GB or more, depending on the
file system.</p>
