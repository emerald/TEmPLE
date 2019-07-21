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
execution of an Emerald program.

Constant Declarations
---------------------

A constant declaration declares a name to correspond to the evaluation
of an expression. Optionally, the programmer can also specify the type
that they expect the resulting value to conform to:

*constDecl* ::= **const** *id* [ **:** *type* ] **<-** *expr*

The Emerald compiler will analyse the expression to infer its type. If
the actual type does not conform to the expected type, a compile-time
type error results. Hence, Emerald is a statically-typed language,
with (limited) type inference.
