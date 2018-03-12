## dot editor macros dot dee

![emacs](https://raw.githubusercontent.com/karahobny/.emacs.d/master/emacs.png)

#### table of contents
+ [style](#style)
+ [packages](#packages)
+ [TODO](#TODO)

### style
##### aka idiosyncracies of my configuration
so i put in a lot of unnecessary (progn) and im unnecesarily obsessed with formatting text.
the (progn)'s are really there to give something from which to auto-indent to, nothing more.
i dont even care if they're costing me startup-time
(but i really dont think thats the case, i might be wrong).

also the (with-no-warning)'s are there also to ease my ocd, but
also due to indentative ａｅｓｔｈｅｔｉｃｓ.

### packages

This configuration file depends on several packages I heartily recommend:
+ use-package
+ helm:
	+ helm-cider
+ company:
    + company-distel
    + company-shell
    + slime-company
+ programming modes: 
	+ ML:
    	+ tuareg
    	+ sml-mode
	+ clojure:
    	+ clojure-mode
    	+ cider
    + scheme / lisp
    	+ geiser
        + slime
    + misc:
    	+ erlang-mode
+ flycheck/make:
	+ flymake:
	    + flymake-shell
    + flycheck:
        + flycheck-haskell
        + flycheck-checkbashims
        + flycheck-clojure
+ eshell:
    + eshell-bookmark
+ lost in sea of parentheses:
	+ parinfer
	+ paredit
+ magit & magithub
+ visual configuration:
    + nlinum
    + cyphejor
    + diminish
    + doom-themes
    + airline-themes
+ mingus
+ which-key
+ paradox
+ undo-tree
+ multiple-cursors

And many more... (to be updated)

### TODO
- [ ] helm looks really dope, gotta get more into it
- [ ] fix some broken shit
- [ ] test some broken shit
- [ ] look for more ways to shave off time from startup initialization

> yeah yeah i know about emacs --daemon and emacs-client
> but mostly i need a way to start quickly to test my configs are in order.

- [ ] the initialization loop optimization
	- [ ] dolist vs mapc(ar) -variants.

> apparently the c-builtins are what i should be looking for, but `dash.el` had
> some nice benchmarks considering it consists solely of higher-order functions.

- [ ] get into org-mode, there's got to be some essence behind its hype
- [ ] perhaps most of all what i should do is code more and fiddle less with editor configuration.
- [ ] fix markdown-preview-mode!

