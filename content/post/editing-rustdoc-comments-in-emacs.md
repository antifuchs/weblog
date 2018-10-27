+++
Title = "Editing rustdoc comments in emacs"
Tags = ["rust", "emacs"]
Categories = ["Hacks"]
date = "2018-10-27T01:50:00+02:00"
+++

I've been writing a [bunch](https://crates.io/crates/ratelimit_meter)
[of](https://crates.io/crates/nonzero_ext)
[rust](https://crates.io/crates/chars)
[code](https://crates.io/crates/cargo-template-ci) lately, and it's
been a pretty great experience! The thing I enjoy most about it is
that the documentation looks just [so extremely
good](https://docs.rs/ratelimit_meter/4.0.1/ratelimit_meter/).

Which brings me to my major point of frustration with my rust-writing
setup: Writing doc comments in emacs's otherwise excellent
[`rust-mode`](https://github.com/rust-lang/rust-mode) is a pain. You
always have to insert the doc comment character sequence de la ligne,
and writing doctest examples was even worse: You write rust code,
inside markdown, in rust comments. Add
[smartparens](https://github.com/Fuco1/smartparens) and other helper
packages, and editing gets really annoying pretty fast.

So, I decided to look around for solutions, and found something pretty
cool: [Fanael's
`edit-indirect`](https://github.com/Fanael/edit-indirect) is an emacs
package that will take lines from the current buffer, put them into a
new buffer, transform them, apply a major mode, and then let you edit
them. When you're done, you apply the changes back to the original
buffer. If this sounds like `org-edit-src-code`, that's because it's
directly inspired by it. (-:

So I wrote [this piece of elisp
glue](https://gist.github.com/antifuchs/aa9fa4c3d1354ea163bc13e63d32db1a)
to help my rustdoc editing experience, and so far it's pretty great:
Navigate to a rustdoc comment, hit `C-c '` (the same keys you'd use in
a literate org file), up pops a buffer in markdown-mode; edit that and
then hit `C-c '` again to apply the changes back to the original
buffer. Easy!

If you write rust in emacs, I hope you'll try this out and if you do,
[let me know](mailto:asf@boinkor.net) how it works for you!
