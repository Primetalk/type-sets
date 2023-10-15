package org.primetalk.tset

import scala.compiletime.ops.boolean._
import scala.compiletime.ops.int._

// Here we define core operations on type sets
sealed trait TSetBase {

  sealed trait TSet

  sealed trait Empty extends TSet

  type ∅ = Empty // \u2205 - synonyms
  sealed trait Singleton[E] extends TSet

  sealed trait Universum extends TSet

  sealed trait Union[A <: TSet, B <: TSet] extends TSet

  type ∪[A <: TSet, B <: TSet] = Union[A, B] // \u222A

  sealed trait Intersection[A <: TSet, B <: TSet] extends TSet

  type ∩[A <: TSet, B <: TSet] = Intersection[A, B] // \u2229

  /**
   * A possible variant:
   * {{{
   *   A \ B = A ∩ Not[B]
   * }}}
   */
  sealed trait Difference[A <: TSet, B <: TSet] extends TSet

  type -[A <: TSet, B <: TSet] = Difference[A,B]

  type Not[A <: TSet] = Difference[Universum, A] // Universum \ A
  type ¬[A <: TSet] = Difference[Universum, A] // u00AC

  /** XOR
   * {{{
   *   A ^ B = A ∪ B \ A ∩ B
   * }}}
   */
  sealed trait SymmetricDifference[A <: TSet, B <: TSet] extends TSet

  type ^[A <: TSet, B <: TSet] = SymmetricDifference[A, B]
  /** A possible variant:
   * {{{
   *   e +: S = {e} ∪ S
   *   sealed trait Insert[E, S <: TSet] extends TSet
   * }}}
   * Looks like we don't often use `Insert`, so we may replace it according to the above equation.
   */

  type Insert[E, S <: TSet] = Union[Singleton[E], S]

  type Set1[A] = Singleton[A]
  type Set2[A, B] = Singleton[A] ∪ Singleton[B]
  type Set3[A, B, C] = Singleton[A] ∪ Singleton[B] ∪ Singleton[C]
  type Set4[A, B, C, D] = Set3[A, B, C] ∪ Singleton[D]

}

sealed trait TSetProperties extends TSetBase {
  type BelongsTo[Element, S <: TSet] <: Boolean = S match
    case Empty => false
    case Universum => true
    case Singleton[Element] => true
    case Singleton[e] => false
    case Union[a, b] => BelongsTo[Element, a] || BelongsTo[Element, b]
    case Intersection[a, b] => BelongsTo[Element, a] && BelongsTo[Element, b]
    case Difference[a, b] => BelongsTo[Element, a] && ![BelongsTo[Element, b]]
    case SymmetricDifference[a, b] => BelongsTo[Element, Union[a,b]] && ![BelongsTo[Element, Intersection[a, b]]]
  infix type ∊[Element, Set <: TSet] = BelongsTo[Element, Set]
  /** Applies `F` to each element of the set. */
  type Map[S <: TSet, F[_]] <: TSet = S match
    case Empty => Empty
    case Universum => Universum
    case Singleton[e] => Singleton[F[e]]
    case Union[a, b] => Union[Map[a,F], Map[b,F]]
    case Intersection[a, b] => Intersection[Map[a,F], Map[b,F]]
    case Difference[a, b] => Difference[Map[a,F], Map[b,F]]
    case SymmetricDifference[a, b] => SymmetricDifference[Map[a,F], Map[b,F]]

  type If[cond <: Boolean, d, e] <: d | e = cond match
    case true => d
    case false => e

  type ForAll[S <: TSet, P[_] <: Boolean] <: Boolean = S match
    case Empty => true
    case Universum => Nothing
    case Singleton[e] => P[e]
    case Union[a, b] => ForAll[a, P] && ForAll[b, P]
    case Intersection[a, b] => 
      ForAll[a, [e] =>> If[BelongsTo[e, b], P[e], true]]
    case Difference[a, b] => 
      ForAll[a, [e] =>> If[BelongsTo[e, b], true, P[e]]]
    case SymmetricDifference[a, b] => 
      ForAll[a, [e] =>> If[BelongsTo[e, b], true, P[e]]]
      &&
      ForAll[b, [e] =>> If[BelongsTo[e, a], true, P[e]]]

  type Exists[S <: TSet, P[_] <: Boolean] =
    ![ForAll[S, [e] =>> ![P[e]]]]

  type IsSubSetOf[A <: TSet, B <: TSet] = 
    ForAll[A, [a] =>> BelongsTo[a, B]]
  type ⊂[A <: TSet, B <: TSet] = IsSubSetOf[A, B]
  type <=[A <: TSet, B <: TSet] = IsSubSetOf[A, B]

  type Equal[A <: TSet, B <: TSet] = IsSubSetOf[A, B] && IsSubSetOf[B, A]

  type FoldLeft[S <: TSet, Res, Z <: Res, Combine[_ <: Res,_] <: Res] <: Res = S match
    case Empty => Z
    case Universum => Nothing
    case Singleton[e] => Combine[Z, e]
    case Union[a, b] => 
      FoldLeft[b, Res, 
        FoldLeft[a, Res, Z, Combine], 
        [r, el] =>> If[BelongsTo[el, a], r, Combine[r, el]]
      ]
      // alternative implementation:
      // FoldLeft[b, Res, 
      //   FoldLeft[Difference[a,b], Res, Z, Combine], 
      // Combine]

    case Intersection[a, b] => 
      FoldLeft[a, Res, Z, [r, el] =>> If[BelongsTo[el, b], Combine[r, el], r]]
    case Difference[a, b] => 
      FoldLeft[a, Res, Z, [r, el] =>> If[BelongsTo[el, b], r, Combine[r, el]]]
    case SymmetricDifference[a, b] => 
      FoldLeft[b, Res, 
        FoldLeft[a, Res, Z, 
          [r, el] =>> If[BelongsTo[el, b], r, Combine[r, el]]
        ], 
          [r, el] =>> If[BelongsTo[el, a], r, Combine[r, el]]
      ]
  type FoldRight[S <: TSet, Res, Z <: Res, Combine[_, _ <: Res] <: Res] <: Res = S match
    case Empty => Z
    case Universum => Nothing
    case Singleton[e] => Combine[e, Z]
    case Union[a, b] => 
      FoldRight[a, Res, 
        FoldRight[b, Res, Z, Combine], 
        [el, r] =>> If[BelongsTo[el, b], r, Combine[el, r]]
      ]
    case Intersection[a, b] => 
      FoldRight[b, Res, Z, [el, r] =>> If[BelongsTo[el, a], Combine[el, r], r]]
    case Difference[a, b] => 
      FoldRight[a, Res, Z, [el, r] =>> If[BelongsTo[el, b], r, Combine[el, r]]]
    case SymmetricDifference[a, b] => 
      FoldRight[b, Res, 
        FoldRight[a, Res, Z, 
          [el, r] =>> If[BelongsTo[el, b], r, Combine[el, r]]
        ], 
          [el, r] =>> If[BelongsTo[el, a], r, Combine[el, r]]
      ]

  type Cardinality[S <: TSet] = 
    FoldLeft[S, Int, 0, [accum <: Int, el] =>> accum + 1]

  type Bottom[S <: TSet] = FoldLeft[S, Any, Any, &]
  type Upper[S <: TSet] = FoldLeft[S, Any, Nothing, |]
  type ToTuple[S <: TSet] = FoldRight[S, Tuple, EmptyTuple, *:] // [el, t <: Tuple] =>> el *: t]
}

sealed trait TSetRender extends TSetProperties {

  // /** Collects runtime elements of the set
  //  *
  //  * @tparam Up upper bound of all elements
  //  */
  // sealed trait Render[Up, A <: TySet] {
  //   def elements: Set[Up]
  // }

//  transparent inline def render[T <: TSet](t: T): Set[Upper[T]] = t match
}
object TSets extends TSetRender
