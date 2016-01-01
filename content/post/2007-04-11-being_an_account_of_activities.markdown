---
date: 2007-04-11T01:59:09Z
mt_id: 82
title: Being an account of activities at ILC07
url: /archives/2007/04/being_an_account_of_activities.html
atom_id: /archives/2007/04/being_an_account_of_activities
---

[Xach nudges](http://xach.livejournal.com/112763.html), I comply. To summarize the text that is to come, the conference was great. Hooray for Nick Levine, whose conference organization talents are deeply awesome.

The setup
---------

I had chosen the worst possible time to fly: a 23:45 (local time) arrival on Saturday guaranteed that I would have to wait 1 hour for the last bus to Cambridge, then arrive there at around 2:00. One bus broke down before that journey started, and a longer-than-expected walk across Cambridge later (which was packed full with drunk english teenagers -- one of them was trying to beat up buildings!), I finally entered the land of Nod at around 3:30.

Tutorials
---------

I managed to wake up on time for breakfast, after which the tutorials started. I stayed on Edi's [Hunchentootorial](http://www.international-lisp-conference.org/2007/tutorials#building) until the second break, then switched, intrigued by [rumors about CMUCL internal discussion](http://jsnell.iki.fi/blog/archive/2007-04-01-ilc-2007-b-mps-tutorial.html), to the [MPS thing](http://www.international-lisp-conference.org/2007/tutorials#mps). The afternoon, I stayed on [Duane's Allegro CL debugging/optimization tutorial](http://www.international-lisp-conference.org/2007/tutorials#optimize). From this one, I took away a few neat things to do in an ACL instance when things are bad. At the end, there was an interesting discussion about memory management techniques (which were to be repeated during [Jans Aasman's](http://www.international-lisp-conference.org/2007/speakers#aasman_jans) talk on the last day). I had dinner at a great Indian place with a crowd of people whose names and faces were forgotten due to me being horribly tired. I think Luke was there. I fell into bed at 10pm and slept for a good, long time.

Favorite quote from Sunday's sessions (Duane Rettig):

> Paul Dietz' test suite doesn't look kindly to adding arguments to standard functions.
>
> It doesn't stop us from doing so, but it does give me pause.

The talks
---------

The days of the talks are a blur. Things that stuck were:

* The weird context switch performed during [Herbert Stoyan's talk](http://www.international-lisp-conference.org/2007/speakers#stoyan_herbert) - From EVAL/APPLY history to code identity transforms in under one minute. (With unbalanced parens on the first such slide. Ow.)

* That, upon hearing about ESA, people seem to worry about compatibility with Emacs applications (don't worry, [Christian](http://clynbech.livejournal.com/2902.html), you were not the only one). Guys, an editor's *text buffers* are not ideal places for an interactive user interface, however compelling the idea might have been in the **seventies.**

* There was a great debate whether a creative angle for teaching CS (as exhibited by [Alexander Repenning's XMLisp](http://www.international-lisp-conference.org/2007/speakers#repenning_alexander) method) was superior to the entirely technological (some might say technocratic) method employed by [Queinnec](http://www.international-lisp-conference.org/2007/speakers#queinnec_christian). As Queinnec's approach seems to balance out genders, scales to large numbers of students, and compensates for not-entirely-motivated teaching personnel, I, personally, am slightly in favor of doing things in the first semesters his way.

* Had a slight case of reality distortion when, during [Michael Sperber's presentation](http://www.international-lisp-conference.org/2007/speakers#sperber_michael), there erupted a discussion about square brackets. A discussion about square and round brackets. At a Lisp conference.

* Felt slightly peeved by [Antonio Menezes Leitao](http://www.international-lisp-conference.org/2007/speakers#leitao_antonio)'s motivational ("pessimistic") pep-talk, which postulated that lisp libraries are poorly documented, badly tested and don't solve [the whole problem](http://xach.livejournal.com/83882.html). While technically impressive, his translator from Java to CL also creates badly-documented libraries (`#| |#`-style comments instead of docstrings because they clutter up the code), which fail tests, and don't solve the whole problem any more than do the Java libs. On the plus side, widespread use of this translator could mean a surge in cool lisp code refactoring tools, and I'd like to see that.

* Props to Cyrus for blatantly ignoring the timekeeper. Those 10 (13, actually) lessons for lisp would have been a great topic for an invited talk, it's too bad he had so little time to expand on them.

* [Charlotte Herzeel's talk](http://www.international-lisp-conference.org/2007/speakers#herzeel_charlotte) was an eye-opener for me and (it seems) many others who hadn't thought much about AOP before. Declarativeness for application behavior descriptions = win.

* Was impressed that not once in his talk did [Andrew Borden](http://www.international-lisp-conference.org/2007/speakers#borden_andrew) utter the word "Lisp" or anything related to it. People with more mathematical skills than me might have gotten something from it.

* The ALU meeting. This was a once-in-a-lifetime experience, in that I don't want to attend one ever again. Interesting fact: The ALU board had 10 members (not sure how many there are after the election). It must be hard to coordinate so many people. Do you need all of ten to maintain an association whose stated purpose is organizing ILCs?

* The Banquet was pretty great. Lots of fun was had, in addition to very good food. I sat next to [Richard Jones](http://www.international-lisp-conference.org/2007/speakers#jones_richard), but was too awed to talk much about things of importance with him. I hear [Juho](http://jsnell.iki.fi/blog/) did bounce some ideas off him later on, which is good.

* Deeply impressed by [HOP](http://www.international-lisp-conference.org/2007/speakers#serrano_manuel). Also, amused that Serrano, as a french native, chose a name that starts with #\H.

* Happy that more vendors are enthusiastic about extensible sequences. Heads up to would-be improvers of Common Lisp: it's possible to add good things to CL that don't add (much) incompatibility with the spec, but retain usefulness. Looking forward to a [CDR document](http://cdr.eurolisp.org).

After the conference
--------------------

On Wednesday, Nick announced that there would be four places on the [Bus tour](http://www.international-lisp-conference.org/2007/tours) to Anglesey Abbey and Ely were available and already paid for; that was too good an opportunity to pass on. (I later learned that one of the places was Patrik's, which means I'll have to invite him to a few drinks at the metalab in exchange. (-:)

Also, on Wednesday evening, Robert Strandh organized a dinner for Nick and a few others at [Hotel Felix](http://www.hotelfelix.co.uk/), for the invitation to which I'm very grateful. Great conversation was had, along with very tasty food, and a lot of (I hear) good wine. (-:

Both Anglesey Abbey and Ely (Thursday) were remarkably beautiful (the great weather didn't hurt, either). Also, our tour guide managed to keep us all awake on the bus to and from each place with stories about each place, which I think was no small feat. Some photos of this tour are available on the [ilc07 flickr group](http://flickr.com/groups/ilc07/).

After the tour, Nick announced that one more place in a punt was available. As nobody else was interested, I gladly took that place, too, and had a great ride, taking pictures until the camera's battery died. Unfortunately, I didn't make it to the appointed place at the appointed time in order to meet Robert, Cathleen and Cyrus for dinner that evening. Too bad, I hear it was really good.

Finally, Friday was a day of packing stuff and walking around Cambridge taking more photos (some of which I might upload later), and flying home. Of special remarkability, I think, are photos of a shop called "TOTAL IT SOLUTIONS" (not an insurance provider), and of a poster announcing Cambridge Stammering Open Day, which I think will finally establish Cambridge as the perfect place to hold conferences on speech impediments.
