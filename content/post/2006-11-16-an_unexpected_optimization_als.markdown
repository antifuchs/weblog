---
date: 2006-11-16T22:55:40Z
mt_id: 59
title: An unexpected optimization (also, archiving IMAP mail with mel-base)
url: /archives/2006/11/an_unexpected_optimization_als.html
---

As I own my own mail server, my mail setup is very much engineered to fit my own needs. This means everything works as I think it should, but it also means I have to hack things myself if I need them. This time, I needed a program that can move IMAP mails to archive mailboxes.

(Side note: My IMAP mail server is [dovecot](http://www.dovecot.org/), which supports keeping mail in different namespaces in separate storage system. For current mail, Maildir is handy: needs no locking, and it's hard to corrupt a mailbox. For archiving, mbox is better, because one mailbox takes up only one file.)

The plan was this: For every mail in "mail.lisp.sbcl.arch", I want it to move the mail to the mail box "archive.&lt;year&gt;.lisp.sbcl.arch". This would have been a tedious mark&drag operation in any gui-based mailer, and hopelessly slow in the emacs-based mailers (which support this operation) like wanderlust. [Mel-base](http://cliki.net/mel-base) to the rescue!

I'll keep it short: [This code](http://boinkor.net/lisp/message-archiver.lisp) was the result. It requires mel-base (obviously), and I only tested it on sbcl.

So, this is how you use it:

0. Install mel-base.
1. Create a file passwords.lisp in the same directory as message-archiver.lisp, with contents like these:

``` cl
(setf *me* "username")
(setf *my-pass* "password")
(setf *my-host* "imap-server")
```
2. load message-archiver.lisp.
3. run `(imap-archiver:archive-messages "lisp.phemlock")` ; and it will move all mails in the mailbox "mail.lisp.phemlock" to the mailbox "archive.&lt;year&gt;.lisp.phemlock", with &lt;year&gt; being the year in the message's Date: header field.

But wait! This is very slow on current versions of mel-base (0.7-2)! Why? The code that copies a message from one folder to another works the same for all folder types: it reads the entire message from the server and sends it back again. As a side effect, this removes all marks. Ow. But we're lucky that Jochen Schmidt is a good hacker and designed mel such that this is easily fixed.

Like everything else in mel-base, operations on a folder have their own protocol, and copying messages from a folder to another has its own generic function, copy-message-using-folders:
```cl
(defgeneric COPY-MESSAGE-USING-FOLDERS (message message-folder sink-folder))
```
As it is, we can easily make a method that specializes on the case where we copy a message from an imap folder to another imap folder. We only need to check that they're on the same server (for that, I assume they're the same if the server name, port, user name and password are the same), and issue the correct UID COPY command to the imap server, and we're good to go:
``` cl
(in-package :mel.folders.imap)
(defmethod copy-message-using-folders :around ((message message) (message-folder imap-folder) (sink-folder imap-folder))
  "Copy a message between two imap folders. We can optimize this case if the folders are on the same server."
  (if (and (equal (host message-folder) (host sink-folder))
           (equal (username message-folder) (username sink-folder))
           (equal (password message-folder) (password sink-folder))
           (equal (imap-port message-folder) (imap-port sink-folder)))
      (progn
        (send-command message-folder "~A uid copy ~A ~A" "UID" (uid message) (mailbox sink-folder))
        (process-response message-folder :on-uid (lambda (m) m)))
      ;; if we're not using the same server, play it safe
      (call-next-method)))
```
Evaluate this, and suddenly everything is 10 times as fast, and marks are preserved after moving the message.

The lesson for today: Good libraries provide functionality that works well enough for the typical use case. Great libraries let you extend them to support your own use cases.
