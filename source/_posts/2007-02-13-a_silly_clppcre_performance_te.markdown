--- 
layout: post
title: A silly CL-PPCRE performance test
mt_id: 74
date: 2007-02-13 19:13:59 -08:00
categories: Lisp
---
I read ["Regular Expression Matching can be Simple and Fast"](http://swtch.com/~rsc/regexp/regexp1.html) today (via [DF](http://daringfireball.net/linked/2007/february#tue-13-cox_regex)), and I immediately had to try its test for exponential-ness with Edi Weitz's excellent [cl-ppcre](http://weitz.de/cl-ppcre/).

Here's a little benchmark thing (as noted in the article, "a?<sup>n</sup>a<sup>n</sup>" translates to e.g. "a?a?a?aaa" for n=3):

```
(defun match-n (n) 
  (let* ((as (make-list n :initial-element #\a))
         (regex (format nil "~{~A?~}~{~A~}" as as))
         (string (make-string n :initial-element #\a))) 
    (time (cl-ppcre:scan regex string))))
```

which gives me (on an intel iMac):

```
CL-USER> (match-n 100)
Evaluation took:
  0.008 seconds of real time
  0.007047 seconds of user run time
  9.4e-5 seconds of system run time
  0 calls to %EVAL
  0 page faults and
  4,562,912 bytes consed.
0
100
#()
#()
```

Tee hee. Perl 5.8.8 has been sitting on the same regex for the last few minutes; according to the article, it'll take 10<sup>15</sup> years. `^C`!

Why is this blog-worthy? Regex benchmarks on bodies of #\a characters aren't very interesting, after all (even if CL-PPCRE defeats Ruby, Perl and pcre, according to the graph). One reason: I really like how easy it was to [format](http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm) the regex from a list of #\as. 

(**Update**: Sure enough, there's a [reddit discussion](http://programming.reddit.com/info/10c60/comments/c10ckp) on the same topic. Note that this post's purpose is to praise format, not cl-ppcre (-;) 
