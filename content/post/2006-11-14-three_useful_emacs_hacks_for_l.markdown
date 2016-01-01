---
categories: ["Lisp"]
date: 2006-11-14T12:50:10Z
mt_id: 57
title: Three useful .emacs hacks for lispers
url: /2006/11/14/three_useful_emacs_hacks_for_l/
---

I gave in and updated my lisp hacking-related emacs configuration today. These three snippets make my life easier now:

* Turning off the "unsafe local variable" warning for common lisp source files. Several packages use the `-*-` convention to set the major mode; some also set variables like Package, Base and Syntax. Emacs 22 doesn't know that they're harmless and prompts almost every time I open a new lisp file. Ugh. Use this:
   
``` cl
(put 'package 'safe-local-variable 'symbolp)
(put 'Package 'safe-local-variable 'symbolp)
(put 'syntax 'safe-local-variable 'symbolp)
(put 'Syntax 'safe-local-variable 'symbolp)
(put 'Base 'safe-local-variable 'integerp)
(put 'base 'safe-local-variable 'integerp)
```
  and remove all those unnecessary `safe-local-variable-values` customizations.

* Wrapping the current defun in (eval-when (:compile-toplevel :load-toplevel :execute) ...): I always misspell at least one of the keywords, so I bound C-c e to wrap the current defun (or the current + prefix-arg - 1 next defuns) in an eval-when:
``` cl
(defun asf-eval-whenify (&optional n)
  (interactive "*p")
  (save-excursion
    (if (and (boundp 'slime-repl-input-start-mark)
             slime-repl-input-start-mark)
        (slime-repl-beginning-of-defun)
        (beginning-of-defun))
    (paredit-wrap-sexp n)
    (insert "eval-when (:compile-toplevel :load-toplevel :execute)\n")
    (slime-reindent-defun)))
(define-key lisp-mode-map [(control c) ?e] 'asf-eval-whenify)
```
  (this requires both [Slime](http://common-lisp.org/project/slime/) and [Paredit](http://www.emacswiki.org/cgi-bin/wiki/ParEdit))

* Fixing Command-left in Aquamacs: A-left (a.k.a. Command-left) goes to the beginning of the line. This is useful (and consistent with OS X) everywhere but in slime REPL mode. Unfortunately, shadowing the binding in the repl mode map isn't possible, as macosx-mode-map's A-left overrules that of the slime repl mode. Here's a piece of advice to work around that:

``` cl
(when (fboundp 'beginning-of-visual-line)
  (defadvice beginning-of-visual-line (around slime-repl-bol first () activate)
    (if (eq major-mode 'slime-repl-mode)
        (call-interactively 'slime-repl-bol)
        ad-do-it)))
```
 *Update:* In Aquamacs 1.0b, you have to use:
``` cl
(when (fboundp 'visual-line-down)
  (defadvice beginning-of-line (around slime-repl-bol first () activate)
    (if (eq major-mode 'slime-repl-mode)
        (call-interactively 'slime-repl-bol)
        ad-do-it))) 
```
