stormy pond
===========

pond is a utility for tracking and managing tasks and other lists. Each
pond contains a collection of items called ripples. Each ripple a summary,
optional description, and may be tagged and grouped using labels called 
reflections.


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

You can optionally specify a summary and reflections.

```bash
# Add a ripple with the summary "I gotta do that thing"
pond add "I gotta do that thing"

# Add a ripple with the reflection "reminder"
pond add "Call your mother" -r reminder
```

If no summary is specified, pond will open your system editor to modify the ripple.

```bash
# Opens your system editor to edit a new ripple
pond add
```

### Editing a ripple

Edit a ripple with `pond edit <id>`. This command will open your system editor to
modify the ripple.

```bash
# Edit the ripple with the ID of "e87a0357778f5f935e0289cf24e091d4ea39c8612a612777cd8efc28b3a4ca4a"
pond edit e87a0357778f5f935e0289cf24e091d4ea39c8612a612777cd8efc28b3a4ca4a
```

### Listing ripples

List all ripples with `pond list`.


### Showing ripple details

View the full contents of a ripple usin `pond show`.

```bash
# Show the contents of ripple with the ID of "e87a0357778f5f935e0289cf24e091d4ea39c8612a612777cd8efc28b3a4ca4a"
pond show e87a0357778f5f935e0289cf24e091d4ea39c8612a612777cd8efc28b3a4ca4a
```
