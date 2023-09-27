# Hagit

Hagit is a git clone that can do some git operations most notably cloning a
repository. In this write-up I go through the journey of creating this clone
using Haskell whilst following the codecrafters course
[build your own git](https://app.codecrafters.io/courses/git/overview). Think of
the course as, you have a task to do, and you research it to implement it.

## Why?

This is mostly done as a fun activity. I wanted to try out how practical Haskell
is to wield in the real world. Haskell has always fascinated me with its
simplicity and elegance when it comes to contrived examples that haskeller's
like to showcase, but I wanted a taste of it beyond that. This goes without
saying, but I am a Haskell beginner as I have observed it from the sidelines
from time to time.

Choice of implementing Git, basically boils down to me being curious about it
and it sounded fun.

## What to expect?

Before continuing I want to line out what you can expect from this article. We
will dig into some of the git internals first by playing around with some git
commands that are not commonly used when using it as a VCS and by chaining these
commands we will get to the functionality of the commands that usually you (dear
git user) interact with. Along the way, we will develop a better understanding
of git internals. Moreover, I will also showcase some important bits from the
Haskell implementation that I ended up with after doing so myself. If you choose
to follow along in your language of choice you will also have a working
toy-clone of git, that you can use as a VCS (barely).

## Prerequisites

- Familiarity with git is required as I won't be explaining what git is and how
  to use it.
- I will deep dive into some of the git internals, so their know how is not
  required.
- If you want to read the implementation as well (which is not required as I
  document the details of each step extensively) then you need to be familiar
  with Haskell.

## Tutorials

1. [Basics](./tutorials/basics.md)
2. [Packfiles](./tutorials/packfiles.md)
3. [Cloning](./tutorials/clone.md)

## Conclusion

We now have a subset of git operations working on our home grown git clone. In
doing so we also got a taste for practical Haskell programming beyond the
basics. From here we can improve upon this implementation quite a bit as we did
skip implementing quite a few features.
