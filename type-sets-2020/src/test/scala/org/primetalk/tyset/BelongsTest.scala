package org.primetalk.tyset

import utest._
import TySets._

class BelongsTest extends TestSuite {

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
      implicitly[a ∊ `{a}`]
      implicitly[b ∊ `{b}`]
    }

    test("BelongsTo should be order insensitive") {
      implicitly[a ∊ `{a,b}`]
      implicitly[a ∊ `{b,a}`]
    }

    test("each element should belong to set of 3 elements") {
      implicitly[a ∊ `{a,b,c}`]
      implicitly[b ∊ `{a,b,c}`]
      implicitly[c ∊ `{a,b,c}`]
    }

    test("union with empty set should be equivalent") {
      type aEmpty = ∅ ∪ `{a}`
      type emptyA = `{a}` ∪ ∅
      implicitly[a ∊ aEmpty]
      implicitly[a ∊ emptyA]
    }

    test("should work for universum") {
      implicitly[a ∊ Universum]
      implicitly[b ∊ Universum]
      implicitly[c ∊ Universum]
    }


      test("not be found for Empty"){
        implicitly[a ∊ `{a}`]
        // "implicitly[A ∊ ∅]" shouldNot compile
      }

      test("not found invalid implicit for singleton"){
        implicitly[a ∊ `{a}`]
//        "implicitly[b ∊ `{a}`]" shouldNot compile
      }

      test("not found invalid implicit for union"){
        implicitly[b ∊ `{a,b}`]
//        "implicitly[c ∊ `{a,b}`]" shouldNot compile
      }

      test("work for union intersection"){
        type `ab ∩ ca` = `{a,b}` ∩ `{c,a}`
        implicitly[a ∊ `ab ∩ ca`]
//        "implicitly[c ∊ `ab ∩ ba`]" shouldNot compile
      }

      test("works for tail Subtract"){
        implicitly[a ∊ `abcd diff bc`]
//        "implicitly[b ∊ `abc \ bc`]" shouldNot compile
//        "implicitly[c ∊ `abc \ bc`]" shouldNot compile
      }

      test("works for head Subtract"){
        implicitly[c ∊ `abcd diff bc`]
//        "implicitly[B ∊ `abc_diff_ab`]" shouldNot compile
//        "implicitly[A ∊ `abc_diff_ab`]" shouldNot compile
      }

      test("works for middle Subtract"){
        implicitly[a ∊ `abcd diff bc`]
        implicitly[d ∊ `abcd diff bc`]
//        "implicitly[B ∊ `abcd_diff_bc`]" shouldNot compile
//        "implicitly[C ∊ `abcd_diff_bc`]" shouldNot compile
      }
  }
}