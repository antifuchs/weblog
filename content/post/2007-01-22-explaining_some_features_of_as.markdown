---
date: 2007-01-22T00:45:16Z
mt_id: 71
title: Explaining some features of asdf-dependency-grovel
atom_id: http://boinkor.net/archives/2007/01/explaining_some_features_of_as
additional_syntax:
- cl
---

Some people asked me why asdf-dependency-grovel (abbreviated adg, to save my fingers) merges systems. Since I wrote it, a few more questions came up, and so I'll try to answer them.

## Why does adg merge systems?

ASDF has a useful dependency-tracking mechanism: if a component changes, it automatically rebuilds that component and all components that depend on it. But what happens to dependencies between systems? They are problematic for ASDF, as it doesn't track the need to recompile components across system boundaries. For example:

``` cl
(defsystem foo
   :components ((:file "a")))

(defsystem bar
   :depends-on (:foo)
   :components ((:file "b")))
```

Suppose that file "a" in system FOO changes; if you load system BAR, file "a" will be recompiled; but file "b" will not! If file "b" uses a macro from file "a", you will load the old version of that macro from FASLs, and things will break. Ow.

There are two solutions for this problem:

* Make ASDF track compilation across system boundaries - expensive, as every user would need to update asdf, and now every user would have to sit through endless recompilation sessions if only a comment in a low-level system changes.
* Merge systems where it makes sense. adg ensures that dependencies are kept up-to-date and minimal: only those things that are actual compile-time (or load-time) dependencies are recorded.

This exact problem is what bit us in mcclim once. I spent a long time thinking about it and came up with adg in the end. So there.

## How does it merge systems?

It merges systems in the same way it operates when not merging systems, really: asdf recursively propagates an instrumened load-op (which translates to a compile-op for uncompiled sources) down the tree of dependencies, which ensures that adg can collect references and definitions. When a definition or a dependency is recorded, adg checks whether it's in a list of "interesting" systems and omits those components and dependencies that are not interesting.

## Why not just use XREF?

XREF is a mechanism provided by many CL implementations which allows queries like `who-calls` and `who-macroexpands`. `who-macroexpands` would cover a good part of compile-time dependencies, but there are many more types of compile-time dependencies! For example:

* Definition of a method that uses a class defined in another file
* Definition of a package that uses another package
* Use of a symbol interned in a package defined in another file
* Calling (at compile or load time) a function defined in another file
* many more...

I do not know a single XREF mechanism that has queries for all these things, and so adg has to grovel to the compiler for them. Thanks to *macroexpand-hook*, they are pretty easy to find out (-:

## Can I have a simple example for using adg?

Sure thing. Assuming you have one (or more) horribly long :serial t system(s):

``` cl
(defsystem something-awful
           :serial t
           :components ((:module "foo" :components (#| lots #| )))
```

You rename the system to indicate that it's the serial one, then add this argument to the defsystem form: `:default-component-type asdf-dependency-grovel:instrumented-cl-source-file`, and replace occurrences of `:module` with `asdf-dependency-grovel:instrumented-module`. The result would look like this:

``` cl
(defsystem something-awful/serial
           :serial t
           :default-component-class asdf-dependency-grovel:instrumented-cl-source-file
           :components ((asdf-dependency-grovel:instrumented-module "foo" :components (#| lots #| )))
```

(If it's more than one system, you have to do the previous step for all the systems that are involved.) Then you define a second system that discovers the dependencies:

``` cl
(defsystem something-awful/dependencies
           :components ((component-file "something-awful"
                         :load-system something-awful/serial ; the system to load. should depend-on all the merged systems.
                         :merge-systems (something-awful/serial) ; if you have more than one system, list them here
                         :cull-redundant t ; remove unnecessary dependencies? Makes for easier-to-read component files
                         :verbose nil ; silly debugging output
                         )))
```

I suggest you put the definitions for the /dependencies system and the /serial system(s) in a separate file, and then, in the original system definition file, define your new `something-awful` system like this:

``` cl
<<<<<<< HEAD
 (defsystem something-awful
   :components
   #.(let ((component-file (make-pathname :name "something-awful-components"
                                          :type "lisp-expr"
                                          :defaults *load-truename*)))
       (when (probe-file component-file)
         (with-open-file (f component-file :direction :input)
           (read f)))))
||||||| parent of fce7929... Apply syntax-highlighting changes to old posts
 (defsystem something-awful
   :components
   #.(let ((component-file (make-pathname :name "something-awful-components"
                                          :type "lisp-expr"
                                          :defaults *load-truename*)))
       (when (probe-file component-file)
         (with-open-file (f component-file :direction :input)
           (read f)))))
=======
(defsystem something-awful
  :components
  #.(let ((component-file (make-pathname :name "something-awful-components"
                                         :type "lisp-expr"
                                         :defaults *load-truename*)))
      (when (probe-file component-file)
        (with-open-file (f component-file :direction :input)
          (read f)))))
>>>>>>> fce7929... Apply syntax-highlighting changes to old posts
```

And that's it. You can now load the separate file, run `(asdf:oos 'asdf:dependency-op :something-awful/dependencies)` and have it emit the component information into something-awful-components.lisp-expr. Done! Your users can now load the new system and hack on it, and ASDF can rely on the dependency information in that file.

You should re-generate the component file (using the :dependency-op) in these cases:

* After hacking on something and incrementally compiling, the system breaks. This probably means that a compile/load-time dependency was introduced somewhere down the line.
* After adding a new file. This requires that you find a sensible place for it in the serial order of the /serial system, then have adg re-generate component info.
