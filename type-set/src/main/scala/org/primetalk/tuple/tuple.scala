package org.primetalk.tuple

import scala.compiletime.error
import scala.compiletime.ops.any.==
import scala.compiletime.ops.boolean._
import scala.compiletime.ops.int.{^ => ^^^, _}

type Empty   = EmptyTuple
type ∅       = Empty
type Set1[a] = a *: EmptyTuple

type ToTuple[T <: Tuple] = T

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

type Union[a <: Tuple, b <: Tuple] <: Tuple = a match
  case EmptyTuple => b
  case el *: tail =>
    BelongsTo[el, b] match
      case true  => Union[tail, b]
      case false => el *: Union[tail, b]

infix type ∪[A <: Tuple, B <: Tuple] = Union[A, B] // \u222A

type Singleton[a]              = Set1[a]
type Set2[a, b]                = Union[Set1[a], Set1[b]]
type Set3[a, b, c]             = Union[Set2[a, b], Set1[c]]
type Set4[a, b, c, d]          = Union[Set3[a, b, c], Set1[d]]

type Intersection[a <: Tuple, b <: Tuple] <: Tuple = a match
  case EmptyTuple => EmptyTuple
  case el *: tail =>
    BelongsTo[el, b] match
      case true  => el *: Intersection[tail, b]
      case false => Intersection[tail, b]

infix type ∩[A <: Tuple, B <: Tuple] = Intersection[A, B] // \u2229

type Difference[a <: Tuple, b <: Tuple] <: Tuple = a match
  case EmptyTuple => EmptyTuple
  case el *: tail =>
    BelongsTo[el, b] match
      case true  => Difference[tail, b]
      case false => el *: Difference[tail, b]

infix type -[a <: Tuple, b <: Tuple] = Difference[a, b]
infix type \[a <: Tuple, b <: Tuple] = Difference[a, b]

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
  case EmptyTuple => true
  case el *: tail => P[el] && ForAll[tail, P]
type ∀[S <: Tuple, P[_] <: Boolean] = ForAll[S,P]
type Exists[S <: Tuple, P[_] <: Boolean] =
  ![ForAll[S, [e] =>> ![P[e]]]]
type ∃[S <: Tuple, P[_] <: Boolean] = Exists[S,P]

type IsSubSetOf[A <: Tuple, B <: Tuple] =
  ForAll[A, [a] =>> BelongsTo[a, B]]

type ⊂[A <: Tuple, B <: Tuple]  = IsSubSetOf[A, B]
type <=[A <: Tuple, B <: Tuple] = IsSubSetOf[A, B]
type >=[A <: Tuple, B <: Tuple] = IsSubSetOf[B, A]

type Equal[A <: Tuple, B <: Tuple] = IsSubSetOf[A, B] && IsSubSetOf[B, A]

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

type Cardinality[T <: Tuple] = Tuple.Size[T]

// builds a function that converts one tuple to another with the same set of types
type Transform[A <: Tuple, B <: Tuple] <: A => B = A match
  case EmptyTuple =>
    B match
      case EmptyTuple => A => B
      case _          => Nothing
  case a *: tailA =>
    B match
      case EmptyTuple   => Nothing
      case `a` *: tailB => Nothing
      case b *: tailB   => Nothing

type Replace[T <: Tuple, A, B] <: Tuple =
  T match
    case EmptyTuple  => EmptyTuple
    case *:[A, tail] => B *: tail
    case *:[a, tail] => a *: Replace[tail, A, B]

type IndexOf0[X <: Tuple, E, N <: Int] <: Int =
  X match
    case EmptyTuple => Nothing
    case E *: xs    => N
    case x *: xs =>
      IndexOf0[xs, E, N + 1]

type IndexOf[X <: Tuple, E] = IndexOf0[X, E, 0]

/** BringToTop moves the given type to head of the tuple. */
type BringToTop[X <: Tuple, E] =
  E *: Tuple.Concat[Tuple.Take[X, IndexOf[X, E]], Tuple.Drop[X, IndexOf[X, E] + 1]]

type SwapTop2[X <: Tuple] <: Tuple = X match
  case EmptyTuple      => EmptyTuple
  case a *: EmptyTuple => a *: EmptyTuple
  case a *: b *: tail  => b *: a *: tail

type Get[El, T <: Tuple] <: El = T match
  case EmptyTuple => Nothing
  case El *: tail => El
  case _ *: tail  => Get[El, tail]

type Getter[El, T <: Tuple] = T => Get[El, T]

type Remove[El, T <: Tuple] <: Tuple = T match
  case EmptyTuple    => Nothing
  case El *: tail    => tail
  case other *: tail => other *: Remove[El, tail]

type Remove2[El, T <: Tuple] = Tuple.Filter[T, [t] =>> t == El]

sealed trait TupleConversions:
  conversions =>

  trait сhecker[A <: Tuple, P[_<: Tuple,_<: Tuple]<:Boolean]:
    def apply[B <: Tuple](b: B)(using P[A, B] =:= true): true = true
  transparent inline def setChecker[A <: Tuple, P[_<: Tuple,_<: Tuple]<:Boolean]: сhecker[A, P] = 
    new сhecker[A, P] {}


  trait setEqualsChecker[A <: Tuple]:
    inline def apply[B <: Tuple](b: B)(using ev: Equal[A, B] =:= true): true = true
    inline def to[B <: Tuple](using ev: Equal[A, B] =:= true): true = true
  transparent inline def setEquals[A <: Tuple]: setEqualsChecker[A] = 
    new setEqualsChecker[A] {}

  transparent inline def setIsASuperset[A <: Tuple] = 
    setChecker[A, IsSubSetOf]

  def Set[T <: Tuple](t: T): T = t


  transparent inline def replace[T <: Tuple, A, B](a: A, b: B)(inline t: T): Replace[T, A, B] =
    inline t match
      case _: EmptyTuple => EmptyTuple
      case p: *:[`A`, _] => b *: p.tail
      case p: *:[_, _]   => p.head *: replace(a, b)(p.tail)

  transparent inline def get[El, T <: Tuple](inline t: T): Get[El, T] =
    inline t match
      case _: EmptyTuple  => error("Requested type not found")
      case p: *:[`El`, _] => p.head
      case p: *:[_, _]    => get(p.tail)

  transparent inline def remove[El, T <: Tuple](inline t: T): Remove[El, T] =
    inline t match
      case _: EmptyTuple  => error("Requested type not found")
      case p: *:[`El`, _] => p.tail
      case p: *:[_, _] =>
        val head *: (tail: Tuple.Tail[p.type]) = p
        head *: remove[El, Tuple.Tail[p.type]](tail)

  transparent inline def replace[T <: Tuple, El, A](a: A, inline t: T): Replace[T, El, A] =
    inline t match
      case _: EmptyTuple  => error("Requested type not found")
      case p: *:[`El`, _] => a *: p.tail
      case p: *:[_, _] =>
        val head *: (tail: Tuple.Tail[p.type]) = p
        head *: replace[Tuple.Tail[p.type], El, A](a, tail)

  inline def extract[B, C <: Tuple]: Conversion[C, (B, Remove[B, C])] =
    c =>
      val b    = get[B, C](c)
      val rest = remove[B, C](c)
      (b, rest)

  inline def convert[A, B, C <: Tuple](using
      extract: Conversion[C, (B, Replace[C, B, A])]
  ): Conversion[A *: C, B *: Replace[C, B, A]] =
    t =>
      t match
        case a *: c =>
          val (b, r) = extract(c)
          b *: r

  inline def convert2[A, B, C <: Tuple]: Conversion[A *: C, B *: Replace[C, B, A]] =
    t =>
      t match
        case a *: c =>
          val b                      = get[B, C](c)
          val rest: Replace[C, B, A] = replace[C, B, A](a, c)
          b *: rest

  // helper type for the function `elementsTo` that constructs a new tuple from an existing one
  type ElementsTo[T <: Tuple, O <: Tuple] <: Tuple =
    O match
      case EmptyTuple => EmptyTuple
      case o *: tail  => Get[o, T] *: ElementsTo[T, tail]
  type ElementsToFunctions[T <: Tuple, O <: Tuple] <: Tuple =
    O match
      case EmptyTuple => EmptyTuple
      case o *: tail  => (T => Get[o, T]) *: ElementsTo[T, tail]

  extension [T <: Tuple](inline t: T)
    inline def elementByType[E]: Get[E, T] =
      conversions.get[E, T](t)
    inline def elementsTo[O <: Tuple](inline o: O): ElementsTo[T, O] =
      inline o match
        case _: EmptyTuple   => EmptyTuple
        case p: *:[h, otail] => get[h, T](t) *: t.elementsTo[otail](p.tail)
    inline def elementsToFunctions[O <: Tuple](inline o: O): ElementsToFunctions[T, O] =
      inline o match
        case _: EmptyTuple   => EmptyTuple
        case p: *:[h, otail] => (t => get[h, T](t)) *: t.elementsTo[otail](p.tail)

sealed trait TupleImplicits extends TupleConversions:
  inline given constructEmpty[T <: Tuple]: Conversion[T, EmptyTuple] =
    t => EmptyTuple

  // WARN: "An inline given alias with a function value as right-hand side can significantly increase
  // generated code size. You should either drop the `inline` or rewrite the given with an
  // explicit `apply` method."
  inline given construct[El, OTail <: Tuple, T <: Tuple](using
      otail: Conversion[T, OTail]
  ): Conversion[T, El *: OTail] =
    t => get[El, T](t) *: otail(t)

  def compatible[T <: Tuple, O <: Tuple](using cvt: Conversion[T, O]): T => O =
    cvt

  
object Tuples extends TupleConversions 
  with TupleImplicits
