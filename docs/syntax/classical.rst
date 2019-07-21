Classical Emerald Syntax
========================

Emerald was conceived in an academic setting, as an object-based
language for distributed computing in the early 1980s. As such, its
classical syntax is inspired by some of the academic giants of the
time, namely `Pascal`_, `Simula`_, and `S`_.

.. _Pascal: https://en.wikipedia.org/wiki/Pascal_(programming_language)
.. _Simula: https://en.wikipedia.org/wiki/Simula
.. _S: https://en.wikipedia.org/wiki/S_(programming_language)

An Emerald program is a sequence of constant declarations. These
constants are initialized in the order that they appear in. There is
no "main method" as such (as you must have in C, C#, Java, etc.). The
in-order initialization of the constants constitutes the entire
execution of an Emerald program. Constants therefore, are of paramount
importance in classical Emerald.

Constant Declarations
---------------------

A constant declaration declares a name to correspond to the evaluation
of an expression. Optionally, you can
specify the type that you expect the resulting value to conform to.
Formally, the syntax is as follows:

*constDecl* ::= **const** *name* [ **:** *type* ] **<-** *expr*

The Emerald compiler will analyse the expression to infer its type. If
the actual type does not conform to the expected type, a compile-time
type error results. Hence, classical Emerald is a **statically-typed
language**, with (limited) type inference. This is akin to C\#.

The correspondence between a *name* and the value that *expr*
evaluates to remains constant throughout the lifetime of a program,
and has global scope. The value itself however, is not constant—it is
mutable, and subject to change throughout the lifetime of a program.
Hence, classical Emerald is an **emperative language**, just like
modern-day C\# and Java.
