---
Categories: ['Hacks']
Description: ""
Tags: ['Rust']
date: 2019-07-27T01:36:37+02:00
title: basename and dirname in Rust
---
I recently did some minor file name munging in Rust, and was reminded that one of the hard parts about learning a new language is the differences in vocabulary.

In UNIX, there are two command line tools, [`basename`](http://pubs.opengroup.org/onlinepubs/009695399/utilities/basename.html) and [`dirname`](http://pubs.opengroup.org/onlinepubs/009695399/utilities/dirname.html). They take a pathname as an argument and print a modified pathname to stdout, which is really handy for shell scripts. Several other languages copied that naming convention, and so I was really surprised to find that googling for [`rust dirname`](https://www.google.com/search?q=rust+dirname&oq=rust+dirname) didn't return anything useful[^google-juice].

Here's a usage example: Say you have a pathname `/etc/ssh/sshd.config`, if you use `dirname` on it, that prints `/etc/ssh` and `basename` prints `sshd.config`. Ruby, python, go all follow a similar pattern (ok, go calls its functions `Dir` and `Base`). Rust does not - it calls them something else[^path-reform].

In Rust, the functions live under the [`Path`](https://doc.rust-lang.org/1.36.0/std/path/struct.Path.html) struct and are called [`parent`](https://doc.rust-lang.org/1.36.0/std/path/struct.Path.html#method.parent) (the `dirname` equivalent), and [`file_name`](https://doc.rust-lang.org/1.36.0/std/path/struct.Path.html#method.file_name) (the `basename` equivalent).

These names make sense! They're just way outside the range of vocabulary I'm used to.

[^google-juice]: Maybe now that this post is published, it will!
[^path-reform]: Rust used to have functions under these names, up until late 2014-early 2015, but then the ["Path reform"](https://github.com/rust-lang/rfcs/pull/474) happened, which normalized the API a great deal and renamed a bunch of functions.
