package org.primetalk.tuple

import scala.compiletime.ops.boolean._
import scala.compiletime.ops.int.{^ => ^^^, _}

type Empty   = EmptyTuple
type Set1[a] = a *: EmptyTuple

type BelongsTo[E, T <: Tuple] <: Boolean =
  T match
    case EmptyTuple  => false
    case `E` *: tail => true
    case _ *: tail   => BelongsTo[E, tail]

infix type ∊[Element, Set <: Tuple] = BelongsTo[Element, Set]

type PrependDistinct[E, T <: Tuple] <: Tuple =
  BelongsTo[E, T] match
    case true  => T
    case false => E *: T

type FoldLeft[S <: Tuple, Res, Z <: Res, Combine[_ <: Res, _] <: Res] <: Res = S match
  case EmptyTuple => Z
  case el *: tail =>
    FoldLeft[tail, Res, Combine[Z, el], Combine]

type FoldRight[S <: Tuple, Res, Z <: Res, Combine[_, _ <: Res] <: Res] <: Res = S match
  case EmptyTuple => Z
  case el *: tail =>
    Combine[el, FoldRight[tail, Res, Z, Combine]]

type Bottom[S <: Tuple] = FoldLeft[S, Any, Any, &]
type Upper[S <: Tuple]  = FoldLeft[S, Any, Nothing, |]

type Union[a <: Tuple, b <: Tuple] <: Tuple = a match
  case EmptyTuple => b
  case el *: tail =>
    BelongsTo[el, b] match
      case true  => Union[tail, b]
      case false => el *: Union[tail, b]

type Singleton[a]              = Set1[a]
type Set2[a, b]                = Union[Set1[a], Set1[b]]
type Set3[a, b, c]             = Union[Set2[a, b], Set1[c]]
type Set4[a, b, c, d]          = Union[Set3[a, b, c], Set1[d]]
type ∪[A <: Tuple, B <: Tuple] = Union[A, B] // \u222A

type Intersection[a <: Tuple, b <: Tuple] <: Tuple = a match
  case EmptyTuple => EmptyTuple
  case el *: tail =>
    BelongsTo[el, b] match
      case true  => el *: Intersection[tail, b]
      case false => Intersection[tail, b]

type ∩[A <: Tuple, B <: Tuple] = Intersection[A, B] // \u2229

type Difference[a <: Tuple, b <: Tuple] <: Tuple = a match
  case EmptyTuple => EmptyTuple
  case el *: tail =>
    BelongsTo[el, b] match
      case true  => Difference[tail, b]
      case false => el *: Difference[tail, b]

type -[a <: Tuple, b <: Tuple] = Difference[a, b]
type \[a <: Tuple, b <: Tuple] = Difference[a, b]

/** XOR
  * {{{
  *   A ^ B = A ∪ B \ A ∩ B
  * }}}
  */
infix type ^[a <: Tuple, b <: Tuple]             = (a ∪ b) \ (a ∩ b)
type SymmetricDifference[a <: Tuple, b <: Tuple] = (a ∪ b) \ (a ∩ b)

type Insert[E, S <: Tuple] = Union[Singleton[E], S]

type Map[S <: Tuple, F[_]] = Tuple.Map[S, F]

type ForAll[S <: Tuple, P[_] <: Boolean] <: Boolean = S match
  case EmptyTuple => false
  case el *: tail => P[el] && ForAll[tail, P]

type Exists[S <: Tuple, P[_] <: Boolean] =
  ![ForAll[S, [e] =>> ![P[e]]]]

type IsSubSetOf[A <: Tuple, B <: Tuple] =
  ForAll[A, [a] =>> BelongsTo[a, B]]

type ⊂[A <: Tuple, B <: Tuple]  = IsSubSetOf[A, B]
type <=[A <: Tuple, B <: Tuple] = IsSubSetOf[A, B]
type >=[A <: Tuple, B <: Tuple] = IsSubSetOf[B, A]

type Equal[A <: Tuple, B <: Tuple] = IsSubSetOf[A, B] && IsSubSetOf[B, A]
