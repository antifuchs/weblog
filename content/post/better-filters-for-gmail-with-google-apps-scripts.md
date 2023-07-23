---
Categories: [Hacks]
Tags: ["gmail"]
date: 2016-01-21T18:57:40-08:00
title: Better filters for gmail with google apps scripts
---

At my workplace, we use github pretty extensively, and with github, we
use organization teams. They allow assigning permissions on different
repos to groups of people, but, are a really great way of
[@-mentioning people](https://github.com/blog/1121-introducing-team-mentions). This
is wonderful, but sadly, github doesn't make it easy for gmail filters
to tell the difference between an email notification that you got
because it was interesting to you, or because somebody sent a heads-up
@-mention to a team you're on.

I thought that was impossible to solve, but I was so wrong!

<!--more-->

## The setup: github notification email basics

Github makes it relatively easy to opt into getting all sorts of
notifications that might interest you. Sadly, it doesn't make it easy
to stop it from notifying you about things that aren't of interest to
you anymore: Either you can't turn off a notification in the first
place, or you have to visit every single thing that it notifies about
and hit "Unsubscribe". Not optimal!

In theory, it should be easier to filter github's notification emails
by relevance than it is to filter on their webface; at least with
emails, you can use third-party filtering tools, right?[^1]

If you're using gmail, you're shaking your head now (as I did): All
the criteria that you could usefully use in gmail filters (From
address, Subject, To address) are the same across all sorts of
notifications you get from github. Ugh.

However, they do set a header field, `X-Github-Reason`: It is set to
`team_mention` if the sole reason you're getting an email is because
somebody mentioned one of your teams (not because you subscribed to an
issue on purpose, say). However, there's a snag: Gmail can't match on
that with its default filters.

Fortunately, [Lyzi Diamond](http://lyzidiamond.com) has written up
[a wonderful, and completely working solution to this problem](http://lyzidiamond.com/posts/github-notifications-google-script/)
using a mechanism that I was vaguely aware of in the past, but didn't
look at in detail:
[Google Apps Scripts](https://developers.google.com/apps-script/?hl=en).

(Go on, [read her article](http://lyzidiamond.com/posts/github-notifications-google-script/); I'll wait.)

## Google Apps Scripts?!

Some time ago, Google made Google Docs, and for some reason they added
a feature where you can edit JavaScript software projects (it's mostly
ok; the editor is no Emacs, but you can get by). And they also added a
facility that lets you trigger those scripts in regular intervals, say
once a minute. And they added
[lots](https://developers.google.com/apps-script/reference/gmail/)
[and](https://developers.google.com/apps-script/reference/calendar/)
[lots of](https://developers.google.com/apps-script/reference/document/)
[bindings](https://developers.google.com/apps-script/reference/drive/)
into their Apps For Business product suite, with much better
functionality than they expose in their
[user-facing APIs](https://developers.google.com/google-apps/)[^2]

In effect, Apps Scripts really powerful cron jobs that google runs for
you, which can process your email.

## My current github notification filter setup

So, as you may have gathered above, I have a Opinions on how a
notification should affect my life:

* If a person in the work org writes in about one of "my" issues or
  pull requests, I would like to know immediately (this means, the
  email should go into my inbox).

* Same if they @-mention me personally. This probably means they're
  blocked, or need help or are asking for a review.

* If somebody @-mentions only a team I'm on, the email should be
  available under a label, but not go into my inbox.

I've modified Lyzi's script for my purposes (also, I made it parse
simple RFC822 headers, but not multi-line ones). The resulting script
is
[in this gist](https://gist.github.com/antifuchs/dde7d18d757d57478ba8).

## Setting this up in your gmail account

This is a pretty manual process, sorry there's no shell script you can
pipe into bash (-:

1. Create a gmail filter to match `from:notifications@github.com` that
   assigns a label (mine is `_github_incoming`) and archives the
   email.  (The google apps script will send github notifications to
   your inbox according to the criteria above!)
2. Create a new script project and copy/paste
   [the script](https://gist.github.com/antifuchs/dde7d18d757d57478ba8)
   from my gist
   [as indicated in Lyzi's blog post](http://lyzidiamond.com/posts/github-notifications-google-script/#neat-how-do-i-make-it-work-with-my-gmail-account). It
   has screenshots! It's great!
3. Adjust the variables at the top to reflect the labels that you want
   email to be tagged with.
4. Set up a trigger: I set mine up like this to call `processMessages` once a minute:
   {{<image alt="trigger to call 'processMessages' once per minute" src="/images/gapps-scripts-triggers.png">}}
5. Set up notifications for that trigger: If anything should go wrong
   (I have a bug, there was a syntax error while pasting), you should
   get a notification. Click on "notifications" and set up a
   notification to email you hourly (or immediately if you like to get
   lots of email in case something goes wrong).

That's it! Now your inbox should accumulate much less clutter!

I am pretty impressed with the things that Apps Scripts can let you
do; my dream is a thing that cleans out email in small batches during
off-hours (since bulk-deleting hundreds of thousands of messages can
render your account unusable for hours). Maybe I'll experiment with
this soon!


[^1]: For my purposes, I'm focusing only on filtering out notifications that I'm getting solely because a team name that I'm on is @-mentioned in a pull request; you could imagine all sorts of other, more complex criteria!

[^2]: Just look at the meager offerings in the public API for [managing gmail filters](https://developers.google.com/admin-sdk/email-settings/?hl=en#manage_filters); you can create filters... and that's it. I could go on about this API for days.
