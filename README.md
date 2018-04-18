## dot editor macros dot dee

![emacs](https://raw.githubusercontent.com/karahobny/.emacs.d/master/emacs.png)

#### table of contents
1. [style](#style)
2. [packages](#packages)

### style
##### aka idiosyncracies of my configuration
so i put in a lot of unnecessary (progn) and im unnecesarily obsessed with formatting text.
the (progn)'s are really there to give something from which to auto-indent to, nothing more.
i dont even care if they're costing me startup-time
(but i really dont think thats the case, i might be wrong).

also the (with-no-warning)'s are there also to ease my ocd, but
also due to indentative *ａｅｓｔｈｅｔｉｃｓ*.

### packages

This configuration file depends on several packages I heartily recommend:
+ use-package
+ helm:
	+ helm-cider
    + helm-google
+ company
+ programming modes:
	+ ML:
    	+ tuareg
            + merlin
            + utop
            + ocp-indent
    	+ sml-mode (`:disabled` due to autoindent not working)
	+ Clojure:
    	+ clojure-mode
        + clj-refractor
    	+ cider
    + Scheme / Racket / Common Lisp:
    	+ geiser
        + racket-mode
        + slime
            + slime-company
    + Haskell: (haven't gotten to test this yet)
      + haskell-mode
      + dante
+ flycheck
    + helm-flycheck
    + flycheck-haskell
    + flycheck-checkbashims
    + flycheck-clojure
    + flycheck-ocaml
+ eshell:
    + eshell-bookmark
+ parentheses handling utilities:
	+ parinfer
	+ paredit
    + rainbow-delimiters
+ magit & magithub
+ visual configuration:
    + nlinum
    + cyphejor
    + diminish
    + doom-themes (doom-one currently in use)
    + spaceline
+ which-key
+ paradox
+ undo-tree
+ comment-dwim-2 & commenter

And many more... (to be updated)
