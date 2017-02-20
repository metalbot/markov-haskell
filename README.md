###Motivation

Kernighan and Pike, in _The Practice of Programming_, devote a chapter to creating Markov based text
generators, on the basis that as a problem it's small enough to be tractable but has enough complexity
to be non-trivial.

I wrote this primarily to see what it might look like in Haskell, a language that I don't know well.

####Usage
The Markov module exports build_ and generate_ methods to build a dictionary and to generate output from
the dictionary respectively.

The _buildFile_ function will generate a dictionary based on a single file, and may be helpful if you want to train
on a single text.

The _buildDirectory_ function will try to process all of the files in a given directory.

To generate output, you can either have the module generate until it reaches an end token (more practical if
you have a lot of small sources) or you can specify a number of words to generate.

saveDictionary and loadDictionary are just helper functions that are wrappers around JSON encode/decode.  Useful
for playing in GHCI since my use case was to process 12k+ movie reviews that were completely static.