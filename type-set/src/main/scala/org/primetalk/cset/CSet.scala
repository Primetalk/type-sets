package org.primetalk.cset

import scala.compiletime.ops.boolean._
import scala.compiletime.ops.int._

sealed trait CSetBase:

  type CSet    = Tuple
  type Empty   = EmptyTuple
  type Set1[a] = a *: EmptyTuple

  type BelongsTo[el, a <: CSet] <: Boolean = a match
    case EmptyTuple   => false
    case `el` *: tail => true
    case a1 *: tail   => BelongsTo[el, tail]

  infix type ∊[Element, Set <: CSet] = BelongsTo[Element, Set]

  type Union[a <: CSet, b <: CSet] <: CSet = a match
    case EmptyTuple => b
    case el *: tail =>
      BelongsTo[el, b] match
        case true  => Union[tail, b]
        case false => el *: Union[tail, b]

  type Singleton[a]            = Set1[a]
  type Set2[a, b]              = Union[Set1[a], Set1[b]]
  type Set3[a, b, c]           = Union[Set2[a, b], Set1[c]]
  type Set4[a, b, c, d]        = Union[Set3[a, b, c], Set1[d]]
  type ∪[A <: CSet, B <: CSet] = Union[A, B] // \u222A

  type Intersection[a <: CSet, b <: CSet] <: CSet = a match
    case EmptyTuple => EmptyTuple
    case el *: tail =>
      BelongsTo[el, b] match
        case true  => el *: Intersection[tail, b]
        case false => Intersection[tail, b]

  type ∩[A <: CSet, B <: CSet] = Intersection[A, B] // \u2229

  type Difference[a <: CSet, b <: CSet] <: CSet = a match
    case EmptyTuple => EmptyTuple
    case el *: tail =>
      BelongsTo[el, b] match
        case true  => Difference[tail, b]
        case false => el *: Difference[tail, b]

  type -[a <: CSet, b <: CSet] = Difference[a, b]
  type \[a <: CSet, b <: CSet] = Difference[a, b]

  /** XOR
    * {{{
    *   A ^ B = A ∪ B \ A ∩ B
    * }}}
    */
  type ^[a <: CSet, b <: CSet]                   = (a ∪ b) \ (a ∩ b)
  type SymmetricDifference[a <: CSet, b <: CSet] = ^[a, b]

  type Insert[E, S <: CSet] = Union[Singleton[E], S]

  type Map[S <: CSet, F[_]] = Tuple.Map[S, F]

  type ForAll[S <: CSet, P[_] <: Boolean] <: Boolean = S match
    case EmptyTuple => false
    case el *: tail => P[el] && ForAll[tail, P]

  type Exists[S <: CSet, P[_] <: Boolean] =
    ![ForAll[S, [e] =>> ![P[e]]]]

  type IsSubSetOf[A <: CSet, B <: CSet] =
    ForAll[A, [a] =>> BelongsTo[a, B]]

  type ⊂[A <: CSet, B <: CSet]  = IsSubSetOf[A, B]
  type <=[A <: CSet, B <: CSet] = IsSubSetOf[A, B]
  type >=[A <: CSet, B <: CSet] = IsSubSetOf[B, A]

  type Equal[A <: CSet, B <: CSet] = IsSubSetOf[A, B] && IsSubSetOf[B, A]

  type FoldLeft[S <: CSet, Res, Z <: Res, Combine[_ <: Res, _] <: Res] <: Res = S match
    case EmptyTuple => Z
    case el *: tail =>
      FoldLeft[tail, Res, Combine[Z, el], Combine]

  type FoldRight[S <: CSet, Res, Z <: Res, Combine[_, _ <: Res] <: Res] <: Res = S match
    case EmptyTuple => Z
    case el *: tail =>
      Combine[el, FoldRight[tail, Res, Z, Combine]]

  type Cardinality[S <: CSet] =
    FoldLeft[S, Int, 0, [accum <: Int, el] =>> accum + 1]

  type Bottom[S <: CSet] = FoldLeft[S, Any, Any, &]
  type Upper[S <: CSet]  = FoldLeft[S, Any, Nothing, |]

  type ToTuple[S <: CSet] = S

object CSets extends CSetBase
