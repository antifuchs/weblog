---
title: "Memorizing passwords with Anki & 1Password"
Tags: ["macos", "anki", "1password"]
Categories: ["Security"]
date: "2018-11-28T01:50:00+02:00"
aliases:
- /2018/11/memorizing-passwords-with-anki--1pssword/
- /2018/11/memorizing-passwords-with-anki-1pssword/
- /2018/11/memorizing-passwords-with-anki-1password/
- /2018/11/memorizing-passwords-with-anki--1password/
---
Recently, I started using [Anki](https://apps.ankiweb.net/), a spaced repetition scheduler[^2], a lot to learn French using the [Fluent Forever](https://fluent-forever.com/) method, and while there have been setbacks, it's been a pretty great experience overall. It seems to be super useful for memorizing and retaining all sorts of information! Since I have to memorize all sorts of passwords (phone unlock code, laptop login password, gym locker combination), why not use 1Password to help me retain them?

Why not, indeed!
<!--more-->

Well, for one, there's the problem of trust: You should always be very careful where you put your passwords. At the moment, I trust my local macOS keychain and [1Password](https://1password.com/) to store (most of) my secrets safely and protect me from breaches. In the instance the anki-web servers have a breach, attackers could read my (unencrypted) passwords! I don't trust the anki-web app that much.

So, since I trust 1Password, is there a way to maybe link from Anki to 1Password so I can see and check the password? Turns out, there is! 1Password has its own URL scheme, pretty well-documented [here](https://github.com/christopherdwhite/iosWorkflows/blob/master/1password.md)[^1].

# Why do this? What passwords would you even memorize?

It's likely that you, the reader likely will have an answer to the question of "which passwords even." There's the obvious one: Your "one" password, the one you use to unlock your password vault - you definitely don't want to forget it, so you'll probably want to use SRS to ensure you don't forget it. The same goes for all the things you have to unlock in order to bootstrap your identity in case of catastrophic data loss - think backup encryption passwords (I use [Arq](https://www.arqbackup.com/)) that you don't type very often but that secure the rest of your data (you might have saved them on a keychain somewhere, but what happens if the computer with that keychain blows up? You'll probably want it in your brain too).

Last but not least, I use this to memorize the kinds of everyday secrets where it's kind of annoying to take out my phone or computer to look them up - credit card and ATM PIN codes, gym locker codes, that sort of thing.

Of course, you should make your memorization job as easy as possible - use [diceware passwords](http://world.std.com/~reinhold/diceware.html) whenever you can[^3]. They'll be much easier to remember that way.

## How to set up that study deck

Setting up the deck is really not that much work, especially as I've pre-made an (empty) Anki deck with a card type that you can import & then start filling out with references to things you need to memorize. Here's a step-by-step guide:

1. In order to use the 1Password URL scheme effectively, you have to set an "advanced" option in 1Password first. In the settings, check the setting to enable "Copy UUID": {{<image alt="the 'advanced settings' window in 1Password" src="/images/anki-1password-advanced.png" >}}

2. Now, identify the passwords that you need to memorize. Ideally, that list is very short. I tagged them "memorize" in 1Password so they all appear in one place, but whatever works for you is best.

3. Download [this anki deck](/assets/anki/memorize-passwords.apkg) and import it into Anki. It should appear as "Memorize Passwords with 1Password" and contain a single example card.

4. Click that deck in the Anki overview and click "Browse". In the new window, hit enter to display the example card.

5. There, you see it has a name and a UUID. That UUID identifies the 1Password entry that you wish to memorize, but isn't itself secret - in the example card, it belongs to a test password that I created for this blog post.

6. Delete that card (it's useless to you, after all!) and close the card browser: {{<image src="/images/anki-1password-delete-example.png" alt="right-click the item and select 'Delete'" >}}

7. Click "Add" in Anki's deck view and give the new entry a name; then, in 1Password, open one of the entries you want to memorize and select "Copy UUID": {{<image alt="The 1Password context menu for a password with 'Copy UUID' selected" src="/images/anki-1password-copy-uuid.png">}}

8. Back in Anki, paste that UUID into the "UUID" field.

Repeat steps 7&8 for all the secrets you want to memorize. Then, let's study!

# How to study these entries

Now comes the magic part: When you study that deck, Anki will ask you what the password is for the name you have given the card (say, your disk encryption password). Then, when you reveal the answer, it gives you a link that takes you to 1Password, where you can reveal the password and check that your answer is right. Then, go back to Anki and tell it how well you did. (Got it wrong? Got it right? Was it too easy?)

This works in both the iOS and macOS versions of Anki and 1Password; I haven't yet tested the windows versions, but I suspect/hope they'll work, too - let me know!

Here's how it looks in macOS for me:
{{<video src="/videos/anki-and-1password.mp4" loop="true" muted="true" width="100%" autoplay="true" >}}

All that app switching is a bit of a hassle, but I believe it's the best we can do for now! It sure feels better than storing important credentials in plaintext, and definitely is better than forgetting them!

[^1]: That URL scheme works in both to the iOS and the macOS app!

[^2]: Also called "SRS" for short

[^3]: 1Password has a diceware password generator, use the "Words" password generator mode!
