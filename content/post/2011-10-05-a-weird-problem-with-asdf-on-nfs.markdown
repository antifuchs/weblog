---
categories: ["Lisp"]
additional_syntax:
- cl
comments: true
date: 2011-10-05T00:00:00Z
title: A Weird Problem With ASDF on NFS (and a workaround)
updated: 2011-10-11
aliases:
- /archives/2011/10/a-weird-problem-with-asdf-on-nfs.html
atom_id: http://boinkor.net/archives/2011/10/a-weird-problem-with-asdf-on-nfs
---

Recently, we at Franz have been seeing weird failures when building a
certain ASDF system on NFS: We were intermittently getting
redefinition warnings when loading a system - not all the time, but
more often when we compiled during certain timeslots.

This was a weird one, and I think it would be nice to record what this
is, and how we figured out what's going on (and how we arrived at our
work-around).

**Update 2011-10-11**: Faré
[informs](http://lists.common-lisp.net/pipermail/asdf-devel/2011-October/002196.html)
me that this problem is fixed (all the way, no need for a workaround)
in ASDF 2.017.9!

## The Symptom

We have a system and a specialized operation (`concat-op`, it
generates a single loadable .fasl we can ship) that depends on
`asdf:load-op`. In our build process, we first load the system, then
generate some data with the loaded system, and then perform the custom
operation on the system.

When performing that operation with a source tree that was checked out
on NFS, the `load-op` that it depends on sometimes got performed a
second time: The lisp loaded all the .fasls again, and for some
constructs in some .fasls, signaled a `WARNING`, which made the build
break.

Oddly enough, the failure happened only during certain time slots - we
would see the build work between 3pm and 4:30pm, and starting at 4:30
it failed consistently until it was time to go home. Huh.

## Aside: How ASDF Operates

Not everyone might be familiar with how ASDF works (if you are, feel
free to skip to the next section, or stay and nitpick (-:), so here's
a small primer on what happens when you type `(asdf:load-system :some-system)`.
Here's a little walkthrough:

1. ASDF runs the generic function `traverse` with the system and the
   operation as parameters.

2. `traverse` walks the dependencies of the system and the contents of
   the system itself, and determines which operations are not yet
   done.

   For a `load-op` on a CL source file, `traverse` will try to
   generate a `load-op` for the input-file of that load-op (the .fasl
   file), check if that .fasl file exists, and if it doesn't, then it
   will also generate a `compile-op` for the corresponding .lisp file.

3. As a result, `traverse` returns a list of operations that must be
   performed on each component (or module, or system). For a clean
   source tree, that list looks something like:
       ((compile-op . source-1) (load-op . source-1)
        (compile-op . source-2) (load-op . source-2) ...)

4. `operate` takes that list and just performes each operation on its
   component in order.

All this means that ASDF takes a two-step approach: It first
determines what needs to be done, then does it. All the smarts in ASDF
are in that `traverse` operation and the underlying mechanisms. The
rest is just a dolist.

OK, with that out of the way:

## The Hunt

I'd gotten this error before, but that was when I was running on a
source tree checked out on an NFS-mounted file system on Windows. I
didn't pay it much mind, because, hey, it's the NFS client on Windows.

But then this exact same problem started happening to a client using
two Linux machines as the client and the server. We had a problem.

At first, we suspected that there was an issue with build order (that
result list of `traverse`). This was a blind alley: The files were
loaded in exactly the same order in the failing and working
scenarios. No luck.

The next thing was to instrument `operation-done-p` before performing
the operation, and there we saw what happened: `operation-done-p`
reported that `load-op` had not been performed on a file. But that
file had been loaded into this very same image just minutes before!
Huh?

`operation-done-p` a generic function and has a method that attempts
to handle the most common cases of operations on ASDF components: the
method specialized on `(operation component)`, which does the
following in the branch that applies to `load-op`:

``` cl
(defmethod operation-done-p (operation component)
  (let ((out-files (output-files o c))
        (op-time (component-operation-time o c)))
    (flet ((latest-in ()
             (reduce #'max (mapcar #'safe-file-write-date in-files))))
      (cond
        ;; ...[cut some branches]

        ((not out-files)
         ;; an operation without output-files is probably meant
         ;; for its side-effects in the current image,
         ;; assumed to be idem-potent,
         ;; e.g. LOAD-OP or LOAD-SOURCE-OP of some CL-SOURCE-FILE.
         (and op-time (>= op-time (latest-in))))

         ;; ...[some more branches here]
         )
         ;; [...]
         )))
```

This consults a registry of times when an operation was performed on a
component: `component-operation-time` returns a
[universal-time](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_u.htm#universal_time),
that is a number of seconds, and compares that to the
[file-write-date](http://www.lispworks.com/documentation/HyperSpec/Body/f_file_w.htm)
- also a universal-time - of the input file (the .fasl). After some
tracing, we determined that for some reason, the .fasl file was one
second younger than the time that ASDF thought the `load-op` had been
performed on it. In other words, the compiler had written the file
AFTER
[load](http://www.lispworks.com/documentation/HyperSpec/Body/f_load.htm)
had had a chance to read it. ASDF was reading a file from the future.

This was the time when we started scratching our heads.

First, we wrote a little test program to verify we weren't crazy:

``` c
#include <stdio.h>
#include <sys/fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/stat.h>

int main(int argc, char **argv) {
  int fd;
  struct timeval tv1, tv2;
  struct stat sb1, sb2;

  char buf[1024];

  while (1) {
    gettimeofday(&tv1, NULL);
    printf("%u.%u\n", tv1.tv_sec, tv1.tv_usec);

    fd=open(argv[1], O_WRONLY|O_CREAT|O_TRUNC, 0660);

    write(fd, buf, sizeof(buf));
    write(fd, buf, sizeof(buf));
    write(fd, buf, sizeof(buf));

    gettimeofday(&tv1, NULL);
    stat(argv[1], &sb1);
    close(fd);
    gettimeofday(&tv2, NULL);
    stat(argv[1], &sb2);

    /* never seems to be triggered */
    if (sb2.st_mtime != sb1.st_mtime) {
      printf("mtime changed between last write (%d) and close (%u)\n",
         sb1.st_mtime, sb2.st_mtime);
      exit(1);
    }
    if (sb1.st_mtime > tv1.tv_sec) {
      printf("mtime after last write has a future timestamp (%u > %u)\n",
         sb1.st_mtime, tv1.tv_sec);
      exit(1);
    }
    if (sb2.st_mtime > tv2.tv_sec) {
      printf("mtime after close has a future timestamp (%u > %u)\n",
         sb2.st_mtime, tv2.tv_sec);
      exit(1);
    }
  }

  return 0;
}
```

When we ran it, after a while, at timestamps very close to the
boundary to the next second, we'd get "mtime after close has a future
timestamp". [What. The.](http://www.osnews.com/story/19266/WTFs_m)

We checked that all machines were synchronized with NTP. They were, to
the same machine on the local network. What is going on?

Luckily, my colleague Ahmon has a lot of experience with
[NFS](http://nfsforwindows.com/home). His expertise and ample use of
tcpdump finally provided the final puzzle piece: NFS protocol 3 on
Linux has a feature called "weak cache consistency": information can
be supplied by servers after most NFS calls (e.g., WRITE) and has the
server's take on file attributes (such as mtime). So if the time on
the server is just a tiny bit ahead of the client, the server will
report the file that the client just wrote is from the client's
future.

When one apparently time-traveling file appears in the source tree,
the `traverse` method will consider the system to not have been
loaded, and will reload the .fasl files starting at the time-traveling
file. Anything after that file in the build order could (and did!)
potentially mess up the lisp image. In the best case, it would just
slow down the build a lot by re-loading a ton of .fasl files. Argh.

## Fixing this Mess (aka, the Workaround)

Since ASDF consults a registry of times that a file was loaded, we
decided it would be easiest to alter the method that records this
timestamp: Instead of the current time, it should record whichever is
later: the current time or the timestamp of the file that it loaded.

``` cl
(defmethod perform :after ((operation load-op) (c component))
  (setf (gethash (type-of operation) (component-operation-times c))
    (reduce #'max (cons (get-universal-time)
                        (mapcar #'safe-file-write-date (input-files operation c))))))
```

And that's it - with this method in place, asdf can now accurately
build our system repeatedly, on NFS, even if wtf.c triggers.

## Lessons Learned

That was a pretty fun afternoon spent debugging our build process. As
a result, we got a working build, and a few shiny new ideas in our
heads:

One, a program should never rely on the system time and some file's
creation time being comparable. This just doesn't work anymore in a
distributed system, especially if you're using full seconds to
represent time.

Two, ASDF is pretty flexible (almost to the point of being too
flexible). To diagnose ASDF's internal state, all we had to do was
[trace](http://www.lispworks.com/documentation/HyperSpec/Body/m_tracec.htm)
some functions it defines, and we managed to put this workaround in
without having to deeply modify any of its sources: All it takes is an
additional :after method. Sweet.

And three, the Allegro CL fasl loader is very fast (at least it feels
so to me, coming from SBCL): In that tiny window (less than 0.07
seconds of real time) it would load a pretty substantial .fasl file
and asdf would register it as loaded. That's pretty impressive! (-:
