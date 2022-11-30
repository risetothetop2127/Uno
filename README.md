## Overview

This is an exercise project for students / aspiring programmers to
start learning in Haskell.

Inspired from this exercise:<br />
[http://nifty.stanford.edu/2012/davies-uno/](http://nifty.stanford.edu/2012/davies-uno/)

Additional code where copied/inspired from:<br />
[https://github.com/epsilonhalbe/Uno](https://github.com/epsilonhalbe/Uno)

## Requirements

To work on this project, make sure you have the following:

* GHC 8.0.1
* Cabal
* Stack

I've started developing this on an OS X machine, so there might be
extra steps to make this work in a Linux or a Windows machine.

## Setup

On Mac OS X

First, install Haskell, cabal, and stack.

```
brew cask install haskell-platform
cabal install hake
cabal install hpack
```

Second, go to cloned repo, and let stack do it's thing.

```
cd uno
hake
```

## References

UNO rules:
[http://www.wikihow.com/Play-UNO](http://www.wikihow.com/Play-UNO)
