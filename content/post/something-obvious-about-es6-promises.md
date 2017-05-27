---
Categories: [JavaScript, Teachable Moments]
Description: ""
Tags: []
date: 2017-05-26T18:37:31-07:00
title: Something obvious (in retrospect) about ES6 promises
---

I've been pretty excited about [the new features of EcmaScript 6 (ES6,
or just "modern JavaScript")](https://github.com/lukehoban/es6features)
for a while, but yesterday it really struck me how entirely different
some of them make the experience of writing JS code!

<!--more-->

## First: A promise[^burritos]

[Promises](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise)
are one thing that's new in ES6. They encode, in a neat little state
machine, how an asynchronous action might progress. From the mozilla docs:

> A Promise is in one of these states:
>
> * pending: initial state, not fulfilled or rejected.
> * fulfilled: meaning that the operation completed successfully.
> * rejected: meaning that the operation failed.

A short example of using the (equally
new)
[`fetch`](https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope/fetch) function
(and the equally, equally
new
[arrow function syntax](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Functions/Arrow_functions))
for accessing HTTP content:

``` js
window.fetch('https://boinkor.net/index.xml').
    then((response) => response.text()).
    then((txt) => console.log(txt.split("\n")[0]));
```

Which would make an HTTP request to this blog's Atom feed, returning a
promise; then when that promise resolves with a response, we request
the response body, which returns a promise in turn. When the second
promise resolves, we print the first line of the body.

At first glance, this is much easier to follow than the callback hell
we all had to deal with before. But wait - there's more!

As you'd expect from a properly asynchronous tool, you can
call
[`.then`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then) on
promises even if they're resolved. (Because things might happen faster
or slower than your computer can execute the next JS statement, of
course!)[^then-promise]

And that brings us to the neat thing that I saw for the first time
yesterday.

## WHAT

I was pair-programming with somebody yesterday, and we were musing
about chaining HTTP requests. We'd written a thing that was firing off
all sorts of requests using `fetch` simultaneously, and waited for
them all to resolve
using
[`Promise.all`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/all). However,
we wanted to fire the requests off one after the other.

So, without blinking, my pair writes this code:

``` js
urls.reduce(
    (p, url) =>
        p.then(() => fetch(url).then(handleResponse)),
    Promise.resolve());
```

What. Uh. This does the right thing, but huh? A bunch of insights
have let do this short piece of code:

* [`Promise.resolve()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/resolve) returns a promise that is already in the
  resolved state. But as mentioned before, it can have `.then` called
  on itself[^thenable]. And so will every promise returned by
  `.fetch`.

* `.then` in turn returns a promise, which lets us chain them
  together.

* [`.reduce`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce) will
  run a function across an array's contents and the previous
  function's return value.

And so, using the resolved promise as a zero element, this piece of
code gathers up requests, one after the other.

Kinda amazing.

## Suddenly, Burritos

I made a promise, but allow me to drift off into maths appreciation
briefly: Promises, combined with some algebra (and operators like
`reduce` that take advantage of the algebraic nature of stuff) allow
you to express realistically cool things in a tidy way.

I'd encourage you to go forage in the mozilla docs for more ES6
features (`fetch` alone is worth a lot)! Look
for
[`Object.assign`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/assign) and
other gems!

But more importantly, think about what you could build if you had a
sensible and well-integrated state machine abstraction for your most
complex software task.


[^burritos]: The promise is that I won't use the word "monad"[^except]

[^except]: Well, oops. This time doesn't count. Also, you're reading footnotes.

[^then-promise]: It's worth noting that `.then` also returns a promise.

[^thenable]: it's "thenable", in ES6 parlance, which I find hilarious.
