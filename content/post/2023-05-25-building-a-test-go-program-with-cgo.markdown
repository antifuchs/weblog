---
Categories: ['Hacks']
Description: ""
Tags: ['Golang']
title: "Building a golang program with cgo"
date: 2023-05-25T17:15:25-04:00
draft: false
---
Recently, I needed to debug a [particularly nasty interaction](https://github.com/NixOS/nixpkgs/issues/233821) between two programs, one of which was a go tool. To get further in understanding the issue, I had to compile a little test program with [`cgo`](https://pkg.go.dev/cmd/cgo), the dreaded (by go programmers) compilation mode that allows go programs to call C code. Unfortunately, it's a bit difficult to find out what to concretely do in order to build a program with `cgo`.

<!--more-->

As with all semi-taboo knowledge[^snide-remark-at-guix], there seems to be a strong reluctance in the respective community to providing straightforward guidance on how do what you want (in this case, build a program with `cgo` which is discouraged and intensely disliked but is an integral part of building several popular & working go programs). Well, we'll show them!

There are two main things you need to do: First, explicitly opt out of disabling the usage of the `cgo` compiler by setting `CGO_ENABLED=1` on the compiler's process environment. (This defaults to on, but your environment might have it turned off! Best to explicitly enable it.)

Second, you *also* have to provide the compiler a reason _in code_ to compile your code with cgo: You have to make an FFI call into C. The easiest way that [Andrew](https://ottawa.place/@andrew) pointed me at is to use call a no-op C function from a go `init()` function.

This looks like the following:

```go
package main

/*
int dummy(void) { return 42; }
*/
import "C"

func init() {
    C.dummy()
}
```

In order to make a program that can optionally be compiled with cgo and the native go complier, I structured the cgo-enabling parts such that the init function above lives in a file `enablecgo.go`, and starts with a line that says `// +build enablecgo`. You can find the whole source code [in my bug report repro repo](https://github.com/antifuchs/nixpkgs-bug-repro-flyctl/tree/main/testapp).

To build the app from that repo, you use the following commandlines:

* *without* cgo (native go compiler): `go build ./`
* *with* cgo: `env CGO_ENABLED=1 go build -tags enablecgo`

If you landed on this page, I hope it can help you get further in your debugging journey, and hope that the pain stops soon.

[^snide-remark-at-guix]: Like the existence of [nonguix](https://gitlab.com/nonguix/nonguix) if you want to use non-free software in [Guix](https://guix.gnu.org/); choice tagline being "Please do NOT promote or refer to this repository on any official Guix communication channels", which already tells you that stuff is juicy and there will be tons of terrible drama if it's brought up.
