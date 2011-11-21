# What is cccp-mode?

cccp-mode is an emacs mode for interacting with a Common Collaborative
Coding Protocol server. One such server can be found
[here](https://github.com/djspiewak/cccp). 

# What is the status of cccp-mode?

It's not even in alpha yet: there is no code at all.

# What can I use cccp-mode for?

At the moment, nothing: see "status" above. Ultimately, the idea is to
enable Google Docs-like collaborative editing, where you and any
number of collegues can remotely edit the same document. So you and I
could both be sitting in front of our own machines, with me using
emacs and you using jEdit, modifying different (or even the same)
sections of the same files, and each of us would be able to see the
other's changes appear in (nearly) real time in our own copy of the
document.

# How is this better than sharing an emacs via tmux/screen?

With tmux/screen, all participants are forced to use the same editor,
on the same machine, with the same configuration. In other words,
cccp-mode enables the following scenarios: 

* You and I both use local emacs to edit the same file. You have a
  completely different emacs setup than I do. We may not even be
  running the same version of emacs.
* You and I use completely different editors to modify the same file.
  I use emacs; perhaps you prefer vi (although no vi plugin currently
  exists).
  
# Cool! When will it be ready for me to use?

No idea. Depends on how hard it is and how much time I spend on it.
