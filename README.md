# type-sets

A small library for working with sets of types.

We define type sets through a few operations - 
union, intersection, difference, etc. Then we try to implement
useful properties of the sets using implicits.
For example, `BelongsTo` implicit is available when an element
belongs to the set. We can define new instances of this type
from existing ones. For example, if `e ∊ A` then `e ∊ A ∪ B` 
as well as `e ∊ B ∪ A`.
