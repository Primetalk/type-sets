package org.primetalk.tuple

type BelongsTo[E, T <: Tuple] <: Boolean =
  T match
    case EmptyTuple  => false
    case `E` *: tail => true
    case _ *: tail   => BelongsTo[E, tail]

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
