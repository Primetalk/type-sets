# type-sets

A small library for working with sets of types.

We define type sets through a few operations - 
union, intersection, difference, etc. Then we try to implement
useful properties of the sets using Scala 3 type lambdas and type matching.
For example, `BelongsTo` type is equal to `true` when the element
belongs to the set.
