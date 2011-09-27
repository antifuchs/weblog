--- 
layout: post
title: SBCL autobenching (a.k.a. "boinkmarking") HOWTO
mt_id: 38
date: 2006-01-25 20:20:43 -08:00
---
Finally! A life sign! And it's a lisp post, too!

I've been promising to do this for a long time now. So, without further ado, here's the

Setting up an SBCL benchmark host HOWTO
---------------------------------------

You, too, can run regular SBCL benchmarks (a.k.a
[boinkmarks](http://sbcl-test.boinkor.net)) with the "Autobench"
software package. It's similar in spirit to
[buildbot](http://buildbot.sf.net), but it is focused at benchmarking
cl implementations, so the abstractions are a bit different.

### Getting the software ###

Autobench requires a recent SBCL release. I've built and tested it
with 0.9.8. It also requires [git](http://git.or.cz/) and
[cogito](http://git.or.cz/cogito/).

Also, it requires a strong cpu, lots of diskspace for the build
archive (around 8.7 GB if you build & benchmark all sbcl 0.8 and 0.9
revisions), and a good stomach that doesn't hurt when you run into one
of my stupid bugs.

If you have all that,

* Get&install the latest [git](http://git.or.cz/) distribution. Any
  build (I tested debian packages and macports) should work fine.

* Get the sources and link them to your ~/.sbcl/systems/:
  * `$ cg clone http://sbcl.boinkor.net/git/boinkmarks.git/ autobench`
  * `$ ln -sf $(pwd)/autobench/lisp/autobench.asd ~/.sbcl/systems/`

* Get its dependencies:
  * [common-idioms](http://cliki.net/common-idioms) (asdf-installable)
  * [iterate](http://cliki.net/iterate) (asdf-installable)
  * [net-telent-date](http://cliki.net/net-telent-date) (asdf-installable)
  * [split-sequence](http://cliki.net/split-sequence) (asdf-installable)
  * [pg](http://cliki.net/pg) (get the [CVS](http://common-lisp.net/project/pg/) version!)

Great! You're almost ready to go. If you try loading the autobench system
with asdf now, it will give you an error message telling you to
customize your ~/.autobench.lisp file. And that's just what we're
going to do now.

### Customizing ~/.autobench.lisp for your machine ###

As I mentioned, we're going to create an .autobench.lisp file
now. Here's a sample customisation file for an x86-64 machine:


     ;;; SIMPLE (important) CUSTOMIZABLE VARIABLES.
     ;; The directory where you checked out the autobench source.
     (setf *base-dir* #p"/home/sbcl-arch/autobench/")

     
     ;;; BUILD STRATEGIES
     (defstrategy +sbcl-64+ sbcl (:mode (:arch :x86_64 :features ())))
     (defstrategy +sbcl-32+ sbcl (:mode (:arch :emulated-x86 :features ())))

     (defstrategy +sbcl-64-threaded+ sbcl (:mode (:arch :x86_64 :features (:sb-thread)))
                  :build-p (every-nth-revision 4))
     (defstrategy +sbcl-32-threaded+ sbcl (:mode (:arch :emulated-x86 :features (:sb-thread)))
                  :build-p (every-nth-revision 4))


     ;;; implementations that should be automatically built. supports only sbcl right now.
     (setq *implementations-to-build* `((sbcl :directory #p"/home/sbcl-arch/space/autobench/+newest-64/"
                                              :strategies (,+sbcl-32+ ,+sbcl-32-threaded+
                                              ,+sbcl-64+ ,+sbcl-64-threaded+))))

     
     ;;; POSTGRES DB SSH TUNNEL VARIABLES
     ;; The function used to ensure the database connection can be established.
     ;; This is different from actually establishing the db connection:
     ;; Think of it as a cheap :before method/hook (:
     (setf *db-connection-setup-function* #'ensure-ssh-tunnel-connected)

     ;; The user name to use for connections to the data base.
     (setf *db-default-user-name* "test-db-user")

     ;; The default port for ssh forwards is 5096.
     ;; Uncomment if this port is in use on your autobuild machine.
     ;; (setf *ssh-port* 5096)

     ;; The user name to use for the ssh connection that establishes the tunnel
     ;; to the boinkmarks DB host.
     (setf *ssh-remote-username* *db-default-user-name*) ; typically the same. customize if they're not.

     


While this may look like a lot at first reading, it's nothing compared to, say, `sendmail.cf` (-:

The one really important variable is `*base-dir*`. if you fail to set
it correctly, autobench will autobelch directories left and right. Try
the pathname with probe-file first. (:

I'll cover the build strategies in the next section, and the postgres
DB ssh tunnel variables in the one titled "connecting to the
boinkmarks db".

#### Build Strategies ####

The things that control how SBCL is built are called build strategies. They're defined via defstrategy like so:

     (defstrategy +strategy-name+ implementation-name
                  (;; initargs, for example:
                   :mode (:arch architecture :features (:sb-thread))
                  [:build-p build-p])

This defines a variable bound to a strategy object, which you can then use on *implementations-to-build*.

Valid names for implementation-name (which isn't evaluated, btw) are SBCL only at the moment.

The :mode init argument is pretty interesting. Let's have a closer
look at it. `Architecture` says which arch to build for. SBCL needs a
bit of hand-holding to build in the x86 emulation on amd64
machines. See sbcl.lisp, `build-in-directory/arch` methods for
details. `Features` are the features that should be put into
customize-target-features.lisp during the build process.

Finally, `build-p` is a function that is called with a version number
and returns t or nil, indicating that the release should be built and
benchmarked or not. The function (every-nth-release n) returns a test
function that returns T for every sbcl release and for every n-th cvs
version of SBCL.

Note that you shouldn't change the strategies, once you ran builds and
benchmarks with them, and imported the results into a database.



#### Connecting to the Boinkmarks DB ####

You do that by sending [me](mailto:asf@boinkor.net) a signed e-mail stating:

* Your desired username

* An SSH public key (the private key should have no password and should be installed in the autobenching user's home directory)

* The output of (machine-instance) and (machine-type), when run on an SBCL 0.9.8 (or later) prompt.

* The DEFSTRATEGY forms in your ~/.autobench.lisp (alternatively, you
  state that you don't know what you should put there and then we can
  discuss them (-:)

When you sent me all that, I'll create your account. (For ssh tunnels
only. No shells from me. If you want one, ask the
[cl.net](http://common-lisp.net) or [tech.coop](http://tech.coop)
guys.) Also, I'll perform the required foreign keys voodoo with your
build strategies so that you can import the build results.

When I've created your account, I'll send you an acknowledgement, and
you can start...

### Auto-Building SBCL ###

The SBCL autobuilder uses [the sbcl git
repository](http://sbcl.boinkor.net/gitweb?p=sbcl.git) as a changeset
source. Good thing you got git in the previous steps, right?

* check out (e.g. the first sbcl 0.9.0 revision):
  * `$ git clone http://sbcl.boinkor.net/git/sbcl-beta.git /wherever/sbcl`
  * `$ cd /wherever/sbcl && cg seek sbcl.0.9.0`

* Use `/wherever/sbcl/` as the directory on *implementations-to-build* in your ~/.autobench.lisp.

* Try to build a few revisions on the command line:
  * `$ /autobench-base-dir/scripts/cron-run-benchmarks`
  * Hit ctrl-c if it works. If not, see `/autobench-base-dir/+log/`
    for logs of every command that failed (i.e. returned non-0 exit
    status).
  
* Set up a cron job to execute *base-dir*"/scripts/cron-run-benchmarks" every day on 0:00:
   * `$ crontab -e`
   * add the line `@daily       wherever/scripts/cron-run-benchmarks`

You're done! Now you can watch your build archive grow and (hopefully)
pretty graphs on
[http://sbcl-test.boinkor.net/bench/](http://sbcl-test.boinkor.net/bench/)!

**UPDATE:** The arch archive is defunct; this guide now uses the git repository.
 
