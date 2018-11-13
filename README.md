# The new Emerald Compiler and Runtime (WIP)

[![Build Status](https://travis-ci.org/emerald/emerald.svg?branch=master)](https://travis-ci.org/emerald/emerald)

## Why?

For now, the reasoning is two-fold:

First, while the Emerald programming language offers an [interesting
programming model](https://learn-emerald.org/docs/methodology.html),
it does so with an arcane syntax, semantics, and implementation. This
repository seeks to provide a more up-to-date (as of 2018)
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
