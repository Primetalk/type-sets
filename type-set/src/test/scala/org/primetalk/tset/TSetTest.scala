package org.primetalk.tset


import utest._

import scala.compiletime.ops.any._

class BelongsTest extends TestSuite {
  import TSets._

  val tests: Tests = Tests {
    sealed trait e
    case object a extends e
    type a = a.type
    case object b extends e
    type b = b.type
    case object c extends e
    type c = c.type
    case object d extends e
    type d = d.type

    type `{a}` = Singleton[a]
    type `{b}` = Singleton[b]
    type `{c}` = Singleton[c]
    type `{d}` = Singleton[d]

    type `{a,b}` = `{a}` ∪ `{b}`

    type `{b,a}` = `{b}` ∪ `{a}`
    type `{b,c}` = `{b}` ∪ `{c}`

    type `{c,a}` = `{c}` ∪ `{a}`

    type `{a,b,c}` = `{a,b}` ∪ `{c}`

    type `{a,b,c,d}` = `{a,b,c}` ∪ `{d}`

    type `abc diff bc` = `{a,b,c}` Difference `{b,c}`
    type `abc diff ab` = `{a,b,c}` Difference `{a,b}`
    type `abcd diff bc` = `{a,b,c,d}` Difference `{b,c}`

    test("BelongsTo should work for singletons") {
      summon[a ∊ `{a}` =:= true]
      // the same result can be achieved by:
      assertTrue[a ∊ `{a}`]

      summon[a ∊ `{b}` =:= false]
      assertFalse[a ∊ `{b}`]

      // similarly:
      assertTrue[b ∊ `{b}`]
      assertFalse[b ∊ `{a}`]
    }

    test("BelongsTo should be order insensitive") {
      assertTrue[a ∊ `{a,b}`]
      assertTrue[a ∊ `{b,a}`]
    }

    test("each element should belong to set of 3 elements") {
      assertTrue[a ∊ `{a,b,c}`]
      assertTrue[b ∊ `{a,b,c}`]
      assertTrue[c ∊ `{a,b,c}`]
    }

    test("union with empty set should be equivalent") {
      type aEmpty = ∅ ∪ `{a}`
      type emptyA = `{a}` ∪ ∅
      assertTrue[a ∊ aEmpty]
      assertTrue[a ∊ emptyA]
    }

    test("should work for universum") {
      assertTrue[a ∊ Universum]
      assertTrue[b ∊ Universum]
      assertTrue[c ∊ Universum]
    }


    test("not be found for Empty"){
      assertTrue[a ∊ `{a}`]
      assertFalse[a ∊ ∅]
    }

    test("not found invalid implicit for singleton"){
      assertTrue[a ∊ `{a}`]
      assertFalse[b ∊ `{a}`]
    } 

    test("not found invalid implicit for union"){
      assertTrue[b ∊ `{a,b}`]
      assertFalse[c ∊ `{a,b}`]
    }

    test("work for union intersection"){
      type `ab ∩ ca` = `{a,b}` ∩ `{c,a}`
      assertTrue[a ∊ `ab ∩ ca`]
      assertFalse[c ∊ `ab ∩ ca`]
    }

    test("works for tail Subtract"){
      assertTrue[a ∊ `abcd diff bc`]
      assertFalse[b ∊ `abc diff bc`]
      assertFalse[c ∊ `abc diff bc`]
    }

    test("works for head Subtract"){
      assertTrue[a ∊ `abcd diff bc`]
      assertTrue[d ∊ `abcd diff bc`]
      assertTrue[c ∊ `abc diff ab`]
      assertFalse[b ∊ `abc diff ab`]
    }

    test("works for middle Subtract"){
      assertTrue[a ∊ `abcd diff bc`]
      assertTrue[d ∊ `abcd diff bc`]
      assertFalse[b ∊ `abcd diff bc`]
      assertFalse[c ∊ `abcd diff bc`]
    }

    test("cardinality"){
      assert(constInt[Cardinality[`abcd diff bc`]] == 2)
      assertTrue[Cardinality[`abcd diff bc`] == 2]
      assertTrue[Cardinality[`abc diff ab`] == 1]
      type `ab ∩ ca` = `{a,b}` ∩ `{c,a}`
      assertTrue[Cardinality[`ab ∩ ca`] == 1]
      assertTrue[Cardinality[`{a,b,c,d}`] == 4]
    }
  }
}