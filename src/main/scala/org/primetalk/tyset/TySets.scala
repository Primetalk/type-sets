package org.primetalk.tyset

import scala.annotation.implicitNotFound

// Here we define operations on sets irrespective of any evidences
sealed trait TySetBase {

  sealed trait TySet

  sealed trait Empty extends TySet

  type ∅ = Empty // \u2205 - synonyms
  sealed trait Singleton[E] extends TySet

  sealed trait Universum extends TySet

  sealed trait Union[A <: TySet, B <: TySet] extends TySet

  type ∪[A <: TySet, B <: TySet] = Union[A, B] // \u222A

  sealed trait Intersection[A <: TySet, B <: TySet] extends TySet

  type ∩[A <: TySet, B <: TySet] = Intersection[A, B]

  /**
   * A possible variant:
   * {{{
   *   A \ B = A ∩ Not[B]
   * }}}
   */
  sealed trait Subtract[A <: TySet, B <: TySet] extends TySet

  type Not[A <: TySet] = Subtract[Universum, A] // Universum \ A

  /** XOR
   * {{{
   *   A ^ B = A ∪ B \ A ∩ B
   * }}}
   */
  sealed trait Xor[A <: TySet, B <: TySet] extends TySet

  /** A possible variant:
   * {{{
   *   e +: S = {e} ∪ S
   * }}}
   * Looks like we don't often use `Insert`, so we may replace it according to the above equation.
   */
  sealed trait Insert[E, S <: TySet] extends TySet

  //  type Insert[E, S <: UniSet] = Union[Singleton[E], S]

  /** Applies `F` to each element of the set. */
  sealed trait TyMap[S <: TySet, F[_]] extends TySet

}

sealed trait TyProperties extends TySetBase {

  // Public relation that can be inferred from operations on sets.
  @implicitNotFound("Couldn't prove that the element belongs to the set")
  sealed trait BelongsTo[Element, Set]

  type ∊[Element, Set] = BelongsTo[Element, Set] // \u220A

  @implicitNotFound("Couldn't prove that set is a subset of another set")
  sealed trait IsSubSetOf[A <: TySet, B <: TySet]

  type ⊂[A <: TySet, B <: TySet] = IsSubSetOf[A, B]
  type <=[A <: TySet, B <: TySet] = IsSubSetOf[A, B]
  // Implementation of BelongsTo using subsets
  type BelongsToViaSubset[E, S <: TySet] = IsSubSetOf[Singleton[E], S]

  /** Collects runtime elements of the set
   *
   * @tparam Up upper bound of all elements
   */
  sealed trait Render[Up, A <: TySet] {
    def elements: Set[Up]
  }

  @implicitNotFound("Couldn't prove that each element is subtype of the given one")
  sealed trait EachElementIsSubtype[Up, S <: TySet]

  @implicitNotFound("Couldn't prove that predicate holds true for each element")
  sealed trait ForAll[P[_], S <: TySet]

  @implicitNotFound("Couldn't prove that predicate holds true for at least one element")
  sealed trait Exists[P[_], S <: TySet]

  @implicitNotFound("Couldn't prove that sets are equal")
  sealed trait Equal[A <: TySet, B <: TySet]

  //  {
  //    def aToB(a: A): B
  //    def bToA(b: B): A
  //  }
  @implicitNotFound("Couldn't prove that the set has only one element")
  sealed trait Cardinality1[Up, S <: TySet] {
    // the type of the single element of the set
    type Element <: Up
  }

}

sealed trait BelongsToLowPriority extends TyProperties {
  // Here we define sets in a "non-constructive way". In particular, we
  // use "belongs to" relation as a primary definition of a set.
  // All other operations should define BelongsTo relation.


  // there is no instance of BelongsTo for Empty
  implicit def SingletonBelongsTo[Element]: BelongsTo[Element, Singleton[Element]] = new BelongsTo[Element, Singleton[Element]] {}

  implicit def UnionBelongsToB[Element, A <: TySet, B <: TySet](implicit eb: BelongsTo[Element, B]): BelongsTo[Element, Union[A, B]] = new BelongsTo[Element, Union[A, B]] {}

  // see also high priority

  implicit def IntersectionBelongsTo[Element, A <: TySet, B <: TySet](implicit ea: BelongsTo[Element, A], eb: BelongsTo[Element, B]): BelongsTo[Element, Intersection[A, B]] = new BelongsTo[Element, Intersection[A, B]] {}

  implicit def universum[E]: BelongsTo[E, Universum] = new BelongsTo[E, Universum] {}


  implicit def SubtractBelongsToA[E, A <: TySet, B <: TySet](implicit ea: BelongsTo[E, A]): BelongsTo[E, Subtract[A, B]] = new BelongsTo[E, Subtract[A, B]] {}

  implicit def SubtractBelongsToAB[E, A <: TySet, B <: TySet](implicit ea: BelongsTo[E, A], enb: BelongsTo[E, Not[B]]): BelongsTo[E, Subtract[A, B]] = ???

  implicit def XorBelongsToA[E, A <: TySet, B <: TySet](implicit ea: BelongsTo[E, A]): BelongsTo[E, Xor[A, B]] = new BelongsTo[E, Xor[A, B]] {}

  implicit def XorBelongsToB[E, A <: TySet, B <: TySet](implicit eb: BelongsTo[E, B]): BelongsTo[E, Xor[A, B]] = new BelongsTo[E, Xor[A, B]] {}

  // checking S has lower priority.
  // See also InsertEBelongsTo
  implicit def InsertSBelongsTo[Element, S <: TySet](implicit s: BelongsTo[Element, S]): BelongsTo[Element, Insert[Element, S]] = new BelongsTo[Element, Insert[Element, S]] {}

  // produce runtime evidence when needed
  def runtimeBelongsTo[A, B <: A](b: B, s: Set[A]): Option[BelongsTo[B, Set[A]]] =
    if (s.contains(b)) Some(new BelongsTo[B, Set[A]] {})
    else None

  /** {{{ e ∊ a => f(e) ∊ F(a) }}} */
  implicit def MapBelongsToB[E, A <: TySet, F[_]](implicit ea: BelongsTo[E, A]): BelongsTo[F[E] ,TyMap[A ,F]] = new BelongsTo[F[E] ,TyMap[A ,F]] {}

  //  implicit def runtimeSingleton[E<: S]
}

// fallback to by-element check of subsets.
sealed trait ElementwiseIsSubSetOf extends TyProperties {
  implicit def singletonIsSubset[E, S <: TySet](implicit es: BelongsTo[E, S]): IsSubSetOf[Singleton[E], S] = new IsSubSetOf[Singleton[E], S] {}
  implicit def insertIsSubset[E, A <: TySet, S <: TySet](implicit es: BelongsTo[E, S], as: IsSubSetOf[A, S]): IsSubSetOf[Insert[E, A], S] = new IsSubSetOf[Insert[E, A], S] {}
}

sealed trait IsSubSetOfLowPriority extends ElementwiseIsSubSetOf  {

  // See also reflectivity below for Empty <= Empty
  implicit def EmptyIsSubSetOf[A<:TySet]: IsSubSetOf[Empty, A] = new IsSubSetOf[Empty, A] {}
  //
  implicit def UnionABIsSubsetOfS[A <: TySet, B <: TySet, S <: TySet](implicit as: IsSubSetOf[A, S], bs: IsSubSetOf[B, S]): IsSubSetOf[Union[A,B], S] = new IsSubSetOf[Union[A,B], S] {}

  implicit def subtractFromSmallerIsEmpty[A <: TySet, B <: TySet](implicit ab: IsSubSetOf[A,B]): IsSubSetOf[Subtract[A,B], Empty] = new IsSubSetOf[Subtract[A,B], Empty] {}

  implicit def intersectionOfSingletonsIsEmpty[E1, E2]: IsSubSetOf[Intersection[Singleton[E1], Singleton[E2]], Empty] = new IsSubSetOf[Intersection[Singleton[E1], Singleton[E2]], Empty] {}
  implicit def subtractFromNonIntersectingIsTheSame[A <: TySet, B <: TySet](implicit ab: IsSubSetOf[Intersection[A,B], Empty]): IsSubSetOf[Subtract[A,B], A] = new IsSubSetOf[Subtract[A,B], A] {}
  implicit def SIsSubsetOfUnionAB_A[S <: TySet, A <: TySet, B <: TySet](implicit smabb: IsSubSetOf[Subtract[S, A], B]): IsSubSetOf[S, Union[A,B]] = new IsSubSetOf[S, Union[A,B]] {}

  //TODO  implicit def SIsSubsetOfUnionAB[S <: UniSet, A <: UniSet, B <: UniSet](implicit as: IsSubSetOf[A, S], bs: IsSubSetOf[B, S]): IsSubSetOf[Union[A,B], S] = new BelongsTo[Element, Union[A,B]] {}

  //  sealed trait Union[A <: UniSet, B <: UniSet]
  //  sealed trait Intersection[A <: UniSet,B <: UniSet] <: UniSet

  // A \ B ⊂ C <=  A ⊂ B + C
  //  implicit def aMinusBIsInC[A <: UniSet, B <: UniSet, C <: UniSet](implicit ev: IsSubSetOf[A, Union[B,C]]): IsSubSetOf[Subtract[A,B], C] = new IsSubSetOf[Subtract[A,B], C] {}
  implicit def SIsSubSetOfIntersectionAB[S <: TySet, A <: TySet, B <: TySet](implicit sa: IsSubSetOf[S,A], sb: IsSubSetOf[S,B]): IsSubSetOf[S, Intersection[A,B]] = new IsSubSetOf[S, Intersection[A,B]] {}
  //
  implicit def SubIsSubsetOfA[A<:TySet, B<:TySet]: IsSubSetOf[Subtract[A,B], A] = new IsSubSetOf[Subtract[A,B], A] {}
  implicit def SubAAIsSubsetOfEmpty[A<:TySet]: IsSubSetOf[Subtract[A,A], Empty] = new IsSubSetOf[Subtract[A,A], Empty] {}

  implicit def AIsSubSetOfXor[A<:TySet, B<:TySet]: IsSubSetOf[Subtract[A,B], Xor[A,B]] = new IsSubSetOf[Subtract[A,B], Xor[A,B]] {}
  implicit def BIsSubSetOfXor[A<:TySet, B<:TySet]: IsSubSetOf[Subtract[B,A], Xor[A,B]] = new IsSubSetOf[Subtract[B,A], Xor[A,B]] {}

  implicit def EIsSubSetOfInsert[E, S <: TySet]: IsSubSetOf[Singleton[E], Insert[E,S]] = new IsSubSetOf[Singleton[E], Insert[E,S]] {}
  implicit def SIsSubSetOfInsert[E, S <: TySet]: IsSubSetOf[S, Insert[E,S]] = new IsSubSetOf[S, Insert[E,S]] {}
  implicit def EInsIsSubSetOfE[E, S <: TySet]: IsSubSetOf[Insert[E,Empty], Singleton[E]] = new IsSubSetOf[Insert[E,Empty], Singleton[E]] {}
  implicit def MapIsSubSetOfE[A<:TySet, B<:TySet, F[_]](implicit ab: IsSubSetOf[A, B]): IsSubSetOf[TyMap[A,F], TyMap[B,F]] = new IsSubSetOf[TyMap[A,F], TyMap[B,F]] {}
}

sealed trait IsSubSetOfHighPriority extends IsSubSetOfLowPriority {
  implicit def ReflectivityIsSubSetOf[A <: TySet]: IsSubSetOf[A, A] = new IsSubSetOf[A, A] {}
  implicit def TransitivityIsSubSetOf[A <: TySet, B <: TySet, C <: TySet](implicit ab: IsSubSetOf[A,B], bc: IsSubSetOf[B,C]): IsSubSetOf[A, C] = new IsSubSetOf[A, C] {}
  //  implicit def SingletonIsSubSetOf[Element]: IsSubSetOf[Singleton[Element], Singleton[Element]] = new IsSubSetOf[Singleton[Element], Singleton[Element]] {}

  implicit def UniversumIsSuperSetOf[A<:TySet]: IsSubSetOf[A, Universum] = new IsSubSetOf[A, Universum] {}
  implicit def SIsSubsetOfUnionAB_B[S <: TySet, A <: TySet, B <: TySet](implicit smbba: IsSubSetOf[Subtract[S, B], A]): IsSubSetOf[S, Union[A,B]] = new IsSubSetOf[S, Union[A,B]] {}
  implicit def SubIsSubsetOfNotB[A<:TySet, B<:TySet]: IsSubSetOf[Subtract[A,B], Not[B]] = new IsSubSetOf[Subtract[A,B], Not[B]] {}

  // Universal properties of Intersection
  implicit def intersectionOfTheSameSetIsTheSameSet[S <: TySet]: IsSubSetOf[Intersection[S, S], S] = new IsSubSetOf[Intersection[S, S], S] {}
  // Universal properties of Union
  implicit def UnionOfTheSameSetIsTheSameSet[S <: TySet]: IsSubSetOf[Union[S, S], S] = new IsSubSetOf[Union[S, S], S] {}

  // Universal properties of Empty
  implicit def subtractEmptyIsTheSame[A <: TySet]: IsSubSetOf[Subtract[A,Empty], A] = new IsSubSetOf[Subtract[A,Empty], A] {}
  implicit def intersectEmptyIsEmpty[A <: TySet]: IsSubSetOf[Intersection[A,Empty], Empty] = new IsSubSetOf[Intersection[A,Empty], Empty] {}

}

sealed trait RenderLowPriority extends TyProperties {
  protected final case class RenderImpl[Up, S <: TySet](elements: Set[Up]) extends Render[Up, S]
  implicit def EmptyRender[Up]: Render[Up, Empty] = RenderImpl[Up, Empty](Set())
  implicit def SingletonRender[Up, E<:Up:ValueOf]: Render[Up, Singleton[E]] = RenderImpl[Up, Singleton[E]](Set(valueOf[E]))
  // cannot render Universum
  // cannot render Not[A <: UniSet] = Subtract[Universum, A]
  implicit def UnionRender[Up, A <: TySet, B <: TySet](implicit ra: Render[Up, A], rb: Render[Up, B]): Render[Up, Union[A,B]] = RenderImpl[Up, Union[A,B]](ra.elements ++ rb.elements)
  implicit def IntersectionRender[Up, A <: TySet, B <: TySet](implicit ra: Render[Up, A], rb: Render[Up, B]): Render[Up, Intersection[A,B]] = RenderImpl[Up, Intersection[A,B]](ra.elements.intersect(rb.elements))
  implicit def SubtractRender[Up, A <: TySet, B <: TySet](implicit ra: Render[Up, A], rb: Render[Up, B]): Render[Up, Subtract[A,B]] = RenderImpl[Up, Subtract[A,B]](ra.elements -- rb.elements)
  implicit def XorRender[Up, A <: TySet, B <: TySet](implicit ra: Render[Up, A], rb: Render[Up, B]): Render[Up, Xor[A,B]] = RenderImpl[Up, Xor[A,B]](ra.elements ++ rb.elements -- ra.elements.intersect(rb.elements))
  implicit def InsertRender[Up, E<:Up:ValueOf, S <: TySet](implicit rs: Render[Up, S]): Render[Up, Insert[E, S]] = RenderImpl[Up, Insert[E, S]](rs.elements + valueOf[E])
}

sealed trait RenderMapLowPriority extends RenderLowPriority {
  implicit def MapEmptyRender[Up, F[_]]: Render[Up, TyMap[Empty, F]] = RenderImpl[Up, TyMap[Empty, F]](Set())
  implicit def MapSingletonRender[Up, E, F[_]](implicit me: ValueOf[F[E]], ev: F[E] <:< Up): Render[Up, TyMap[Singleton[E], F]] = RenderImpl[Up, TyMap[Singleton[E], F]](Set(valueOf[F[E]]))
  // cannot render Universum
  // cannot render Not[A <: UniSet] = Subtract[Universum, A]
  implicit def MapUnionRender[Up, A <: TySet, B <: TySet, F[_]](implicit ra: Render[Up, TyMap[A,F]], rb: Render[Up, TyMap[B,F]]): Render[Up, TyMap[Union[A,B], F]] = RenderImpl[Up, TyMap[Union[A,B], F]](ra.elements ++ rb.elements)
  implicit def MapIntersectionRender[Up, A <: TySet, B <: TySet, F[_]](implicit ra: Render[Up, TyMap[A,F]], rb: Render[Up, TyMap[B,F]]): Render[Up, TyMap[Intersection[A,B], F]] = RenderImpl[Up, TyMap[Intersection[A,B], F]](ra.elements.intersect(rb.elements))
  implicit def MapSubtractRender[Up, A <: TySet, B <: TySet, F[_]](implicit ra: Render[Up, TyMap[A,F]], rb: Render[Up, TyMap[B,F]]): Render[Up, TyMap[Subtract[A,B], F]] = RenderImpl[Up, TyMap[Subtract[A,B], F]](ra.elements -- rb.elements)
  implicit def MapXorRender[Up, A <: TySet, B <: TySet, F[_]](implicit ra: Render[Up, TyMap[A,F]], rb: Render[Up, TyMap[B,F]]): Render[Up, TyMap[Xor[A,B], F]] = RenderImpl[Up, TyMap[Xor[A,B], F]](ra.elements ++ rb.elements -- ra.elements.intersect(rb.elements))
  implicit def MapInsertRender[Up, E, S <: TySet, F[_]](implicit rs: Render[Up, TyMap[S, F]], me: ValueOf[F[E]], ev: F[E] <:< Up): Render[Up, TyMap[Insert[E, S], F]] = RenderImpl[Up, TyMap[Insert[E, S], F]](rs.elements + valueOf[F[E]])
}

sealed trait BelongsToHighPriority extends BelongsToLowPriority {
  implicit def UnionBelongsToA[Element, A <: TySet,B <: TySet](implicit ea: BelongsTo[Element, A]): BelongsTo[Element, Union[A,B]] = new BelongsTo[Element, Union[A,B]] {}
  // if we insert element, then it belongs to the set.
  implicit def InsertEBelongsTo[Element, S <: TySet]: BelongsTo[Element, Insert[Element, S]] = new BelongsTo[Element, Insert[Element, S]] {}
}

sealed trait EachElementIsSubtypeLowPriority extends TyProperties {
  implicit def EmptyEachElementIsSubtype[E]: EachElementIsSubtype[E, Empty] = new EachElementIsSubtype[E, Empty] {}
  implicit def SingletonEachElementIsSubtype[Up, E<:Up]: EachElementIsSubtype[Up, Singleton[E]] = new EachElementIsSubtype[Up, Singleton[E]] {}
  implicit def UnionEachElementIsSubtype[Up, A<: TySet, B <: TySet](implicit ea: EachElementIsSubtype[Up, A], eb: EachElementIsSubtype[Up, B]): EachElementIsSubtype[Up, Union[A,B]] =
    new EachElementIsSubtype[Up, Union[A,B]] {}

  implicit def IntersectionBEachElementIsSubtype[Up, A<: TySet, B <: TySet](implicit eb: EachElementIsSubtype[Up, B]): EachElementIsSubtype[Up, Intersection[A,B]] =
    new EachElementIsSubtype[Up, Intersection[A,B]] {}

  implicit def XorEachElementIsSubtype[Up, A<: TySet, B <: TySet](implicit ea: EachElementIsSubtype[Up, A], eb: EachElementIsSubtype[Up, B]): EachElementIsSubtype[Up, Xor[A,B]] =
    new EachElementIsSubtype[Up, Xor[A,B]] {}

  implicit def InsertEachElementIsSubtype[Up, E<:Up, S <: TySet](implicit es: EachElementIsSubtype[Up, S]): EachElementIsSubtype[Up, Insert[E, S]] = new EachElementIsSubtype[Up, Insert[E, S]] {}
}

sealed trait EachElementIsSubtypeHighPriority extends EachElementIsSubtypeLowPriority {
  implicit def IntersectionAEachElementIsSubtype[Up, A<: TySet, B <: TySet](implicit ea: EachElementIsSubtype[Up, A]): EachElementIsSubtype[Up, Intersection[A,B]] =
    new EachElementIsSubtype[Up, Intersection[A,B]] {}

}

sealed trait EqualSets extends TySetBase with IsSubSetOfHighPriority {
  implicit def equalUnionAB[A<:TySet, B<:TySet]:Equal[Union[A, B], Union[B, A]] = new Equal[Union[A, B], Union[B, A]] {}
  //  {
  //    override def aToB(a: Union[A, B]): Union[B, A] = new Union[B, A]{}
  //
  //    override def bToA(b: Union[B, A]): Union[A, B] = ???
  //  }
  implicit def equalUnionEmpty[S<:TySet]:Equal[Union[S, Empty], S] = new Equal[Union[S, Empty], S] {}
  implicit def equalInsertUnion[E, S<:TySet]:Equal[Insert[E, S],Union[Singleton[E], S]] = new Equal[Insert[E, S],Union[Singleton[E], S]]{}
  implicit def equalAB[A<:TySet, B<:TySet](implicit ab: IsSubSetOf[A,B], ba: IsSubSetOf[B, A]): Equal[A,B] = new Equal[A,B] {}

  implicit def insertExistingElement[E, S<:TySet](implicit es: BelongsTo[E, S]): Equal[Insert[E, S], S] = new Equal[Insert[E, S], S] {}
  implicit def insertExistingElement2[E, S<:TySet](implicit es: BelongsTo[E, S]): Equal[Union[Singleton[E], S], S] = new Equal[Union[Singleton[E], S], S] {}
  implicit def mapEq[A<:TySet, B<:TySet, F[_]](implicit eq: Equal[A, B]): Equal[TyMap[A, F], TyMap[B, F]] = new Equal[TyMap[A, F], TyMap[B, F]] {}
}

sealed trait SingletonSets extends TyProperties {
  implicit def singletonCardinality1[Up, E <: Up]: Cardinality1[Up, Singleton[E]]{type Element = E} = new Cardinality1[Up, Singleton[E]]{type Element = E}
  implicit def insert1Cardinality1[Up, E <: Up]: Cardinality1[Up, Insert[E, Empty]]{type Element = E} = new Cardinality1[Up, Insert[E, Empty]]{type Element = E}
}

object TySets extends BelongsToHighPriority
  with IsSubSetOfHighPriority
  with EachElementIsSubtypeHighPriority
  with RenderLowPriority
  with RenderMapLowPriority
  with EqualSets
  with SingletonSets
