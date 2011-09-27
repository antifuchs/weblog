--- 
layout: post
title: "cl-beanstalk: A queue service client"
mt_id: 100
date: 2010-01-31 17:49:52 -08:00
categories: Lisp
---
Over the weekend, I wrote a little client library to a queue server that I've grown very fond of over the last year, [beanstalk](http://kr.github.com/beanstalkd/). It's a very simple queue server, but it comes with a nice feature (delayed jobs) that I've had a use for recently.

The queue server is nicely engineered (written in C, works with queues a few million jobs deep), and very fast; it has guards in the protocol against worker failure, and it was a pleasure to implement: The whole thing is just 320 lines of code, including comments.

You can get the source (and a tiny example) at the [cl-beanstalk](http://github.com/antifuchs/cl-beanstalk) github repository.

Hope this is useful for anyone else - I am planning on using this in autobench myself, to distribute work across several build hosts. 
