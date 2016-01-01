---
date: 2007-01-21T13:58:24Z
mt_id: 70
title: '"make depend" for lisp'
aliases:
- /archives/2007/01/make_depend_for_lisp.html
atom_id: http://boinkor.net/archives/2007/01/make_depend_for_lisp
---

As the software-publishing planet.lisp.org crowd probably knows,
writing simple defsystems with ASDF is pretty easy. Dependencies are
not hard to find (and to specify), if you have up to 10 or 20
components. Beyond that, though, it becomes pretty painful to maintain
a system definition file that doesn't result in a compilation
error. After that, it's easier to use a serial system definition: just
find a defined order to compile and load the files.

There's a tradeoff, though: Serial system definitions are a pain for
users who want to hack on your code. If someone changes a file (e.g. one
close to the start of the series of components), every single component after it in the series must be recompiled. Dependencies would help, but we already
established that they're too hard to maintain by hand in a large system. What's a system definition maintainer to do?

Have the computer do the dirty work, of course (-:

A few months ago, I had a pretty neat idea: To find out the
compile-time dependencies in a system, you'd have to hook into the
compiler. And the compiler provides one such hook:
[*macroexpand-hook*](http://www.lispworks.com/reference/HyperSpec/Body/v_mexp_h.htm)
- of course, all the operators that can construct a compile-time
dependency must be macros - and all the standard operators are!

So, I wrote a program called asdf-dependency-grovel that compiles a
serial asdf system (or an asdf system that just so happens to be in
working order), and extracts components with dependency information.

Here's an outline of what it does for the prime example of a compile-time
dependency: a file uses a macro that is defined in another file:

``` cl
        (defmethod asdf:perform :around ((op asdf:compile-op) (comp asdf:cl-source-file))
          (let* ((old-hook *macroexpand-hook*)
                 (*macroexpand-hook*
                  (lambda (fun form env)
                     (when (listp form)
                       (case (first form)
                         ((defmacro)
                          (signal-macroexpansion 'provides (second form) (first form) comp))
                          ;; many many more form types cut
                         (t (signal-macroexpansion 'uses (second form) (first form) comp))))
                     (funcall old-hook fun form env))))
            (call-next-method)))
```

And all that signal-macroexpansion does is send a little notice to the function that keeps track of dependencies (i.e. it invokes a closure on a hook)
to tell it that there's either a use of a previously defined macro
from the current component, or a new definition from from the current
component.

It has additional handlers for:

* defclass and define-condition ("use" of superclasses, and definition
  of classes for use by defmethod and other defclass forms)
* defpackage and in-package.
* defun - it rewrites the function's macroexpansion into code that signals a compile-time use.
* defmethod and defgeneric ("use" of generic functions and classes on
  which a method specializes),
* defconstant - makes the constant a symbol-macro signals the variable was used and the constant's value.

This code managed to automatically generate a working dependency graph
for mcclim, even merging 8 pretty large systems into one in the
process. The resulting system now contains a total of 168 components
with 192 non-redundant dependencies!

If you want to try your luck with asdf-dependency-grovel, check out the
[cliki page](http://www.cliki.net/asdf-dependency-grovel).

Also, here are two graphs of the new [McCLIM system's dependencies](http://boinkor.net/lisp/mcclim/CLIM-2007.png) and [the CLX system's dependencies](http://boinkor.net/lisp/mcclim/CLX-2007.png).
