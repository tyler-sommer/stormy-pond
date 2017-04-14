stormy pond
===========

pond is a utility for tracking and managing tasks and other lists. Each
pond contains a collection of items called ripples. Each ripple a summary,
optional description, and may be tagged and grouped using labels called facets.


Installation
------------

Currently, you have to use stack or cabal and build pond yourself.

```bash
git clone https://github.com/tyler-sommer/stormy-pond
cd stormy-pond
stack install # or cabal install
```


Usage
-----

Run pond from your terminal with `pond`. There are several commands for
interacting with your data.

### Adding a ripple

Add a new ripple to your pond with `pond add`. 

You can optionally specify a summary and tags, but if no summary is 
specified, pond will open your system editor to modify the ripple.

### Editing a ripple

Not implemented, yet.

### Listing ripples

Not implemented, yet.


Tips
----

Invoking the "add" or "edit" commands without arguments will open your
system editor with a git-style format for modifying ripples.