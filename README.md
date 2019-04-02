# TEmPLE - The Emerald Programming Language Effort

This workbench serves to improve and advance the Emerald programming
language. Out of purely divine considerations, the TeMPLE is currently
written in Haskell.

[![Build Status](https://travis-ci.org/emerald/TEmPLE.svg?branch=master)](https://travis-ci.org/emerald/TEmPLE)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](LICENSE)

## A Re-Implementation of Emerald

First and foremost, I am working on a re-implementation of the Emerald
compiler and runtime.

### Why Re-Implement Emerald?

After all, there already exist [several Emerald
implementations](https://github.com/emerald/src-versions/).

For now, the reasoning is three-fold:

First, while the Emerald programming language offers an [interesting
programming model](https://learn-emerald.org/docs/methodology.html),
it does so with an arcane syntax, semantics, and implementation. This
repository seeks to provide a more up-to-date (as of 2018-2019)
implementation of the Emerald programming methodology.

Second, the [old Emerald
compiler](https://github.com/emerald/old-emerald) is written in
Emerald (i.e., it is a bootstrapped compiler). This is excellent
news—we can be more confident in that Emerald is a truly
general-purpose language. However, the original, non-Emerald
implementation of Emerald, from which Emerald was bootstrapped, is no
longer available. This poses a problem since to run Emerald, you have
to rely on a binary compiled by someone, some time long ago, and which
has been lying around on the Internet since.

To resolve this dilemma, this repository also seeks to provide a
re-implementation of classical Emerald. The (double) litmus test will
be to see if this compiler can compile the old Emerald compiler to an
equivalent binary, and whether both compilers produce equivalent
binaries for some comprehensive suite of test-programs. If so, we will
have *re-bootstrapped* Emerald, and can proceed with confidence to
improve the Emerald programming environment.

Third, the tests developed for this compiler, can serve as additional
tests for the original Emerald compiler. Gradually, this will help
increase confidence in the quality of Emerald.

## Working with Stack

"Stack is a cross-platform program for developing Haskell projects." -
[The Haskell Tool Stack
Documentation](https://docs.haskellstack.org/en/stable/README/)

### Build

    stack build

### Test

    stack test

### Replay tests with a particular QuickCheck seed

    stack test --test-arguments --quickcheck-replay=<seed>

### GHCi

Start up GHCi with the source files loaded:

    stack ghci

Start up GHCi with the test files loaded:

    stack ghci Emerald:Emerald-test
