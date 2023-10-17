package org.primetalk.tset

import utest._

class TSetTest extends TestSuite with Abcd {
  import TSets._

  val tests: Tests = Tests {

    type `{a}`     = Singleton[a]
    type `{b}`     = Singleton[b]
    type `{a,b}`   = Set2[a, b]
    type `{b,c}`   = Set2[b, c]
    type `{a,b,c}` = Set3[a, b, c]
    type `¬{a}`    = ¬[`{a}`]

    test("BelongsTo should work in simple cases") {
      assertTrue[a ∊ `{a}`]
      assertTrue[a ∊ `{a,b}`]
      assertTrue[c ∊ `{a,b,c}`]
      assertTrue[c ∊ (`{a,b,c}` - `{a,b}`)]
      assertTrue[a ∊ (`{a,b}` ^ `{b,c}`)]
      assertTrue[c ∊ (`{a,b}` ^ `{b,c}`)]
      assertFalse[b ∊ (`{a,b}` ^ `{b,c}`)]
      assertFalse[Int ∊ (`{a,b}` ^ `{b,c}`)]
    }

    test("subset should work in simple cases") {
      assertTrue[`{b}` ⊂ `{a,b}`]
      assertTrue[`{b}` ⊂ `{a,b,c}`]
      assertTrue[`{b}` ⊂ (`{a,b,c}` ∩ `{a,b}`)]
      assertTrue[`{b}` ⊂ (`{a,b,c}` ∪ ∅)]

      assertTrue[`{b}` ∪ ∅ ⊂ `{a,b,c}`]
      assertTrue[Equal[`{b}` ∪ ∅, `{b}`]]
      assertTrue[Equal[`{b}`, `{b}` ∪ ∅]]
    }

    test("upper") {
      val a1: Upper[`{a,b}`] = a
      val b1: Upper[`{a,b}`] = b
    }
    test("ToTuple") {
      summon[ToTuple[`{a,b}`] =:= (a, b)]
      summon[ToTuple[`{b}`] =:= b *: EmptyTuple]
    }
  }
}
