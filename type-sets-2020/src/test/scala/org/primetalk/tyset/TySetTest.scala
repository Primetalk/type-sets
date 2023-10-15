package org.primetalk.tyset

import utest._
import TySets._

class TySetTest extends TestSuite {

  val tests: Tests = Tests {
    case object a
    type a = a.type
    case object b
    type b = b.type
    case object c
    type c = c.type

    type `{a}` = Singleton[a]
    type `{b}` = Singleton[b]
    type `{a,b}` = Union[Singleton[a], Singleton[b]]
    type `{b,c}` = Union[Singleton[b], Singleton[c]]
    type `{a,b,c}` = Union[Singleton[a], `{b,c}`]
    type `¬{a}` = Difference[Universum, `{a}`]
    val `a ∊ {a}`: BelongsTo[a, `{a}`] = implicitly[BelongsTo[a, `{a}`]]
    val `a ∊ {a,b}`: BelongsTo[a, Union[Singleton[b], Singleton[a]]] = implicitly[BelongsTo[a, Union[Singleton[b], Singleton[a]]]]
    val `c ∊ {a,b,c}`: BelongsTo[c, `{a,b,c}`] = implicitly[BelongsTo[c, `{a,b,c}`]]
    val `c ∊ {a,b,c} - {a,b}` = implicitly[BelongsTo[c, Difference[`{a,b,c}`, `{a,b}`]]]

    val `a ∊ {a,b} ^ {b,c}` = implicitly[BelongsTo[a, SymmetricDifference[`{a,b}`, `{b,c}`]]]
    val `c ∊ {a,b} ^ {b,c}` = implicitly[BelongsTo[a, SymmetricDifference[`{a,b}`, `{b,c}`]]]

    // the following fails at compile time
    //  val `b   ∊ {a,b} ^ {b,c}` = implicitly[BelongsTo[b,   Xor[`{a,b}`, `{b,c}`]]]
    //  val `Int ∊ {a,b} ^ {b,c}` = implicitly[BelongsTo[Int, Xor[`{a,b}`, `{b,c}`]]]

    val `{b} ⊂ {a,b}` = implicitly[IsSubSetOf[`{b}`, `{a,b}`]](SIsSubsetOfUnionAB_A[`{b}`, `{a}`, `{b}`](subtractFromNonIntersectingIsTheSame[`{b}`, `{a}`]))
    // val `{b} ⊂ {a,b,c}` = implicitly[IsSubSetOf[`{b}`, `{a,b,c}`]]
    // val `{b} ⊂ {a,b,c} intersection {a,b}` = implicitly[`{b}` ⊂ (`{a,b,c}` Intersection `{a,b}`)]//(SIsSubSetOfIntersectionAB)

    val `{b} ⊂ {b} + Empty` = implicitly[IsSubSetOf[`{b}`, Union[`{b}`, Empty]]]
    // val `{b} + Empty  ⊂  {b}` = implicitly[IsSubSetOf[Union[`{b}`, Empty], `{b}`]]
    // val `{b} == {b} + Empty` = implicitly[Equal[`{b}`, Union[`{b}`, Empty]]]
    // val `{b} == Insert b, Empty` = implicitly[Equal[`{b}`, Insert[b, Empty]]](equalAB[`{b}`, Insert[b, Empty]])

  }
}
