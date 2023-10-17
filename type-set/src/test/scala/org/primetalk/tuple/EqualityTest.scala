package org.primetalk.tuple

import utest._

import scala.compiletime.ops.any._

class EqualityTest extends TestSuite with Abcd {
  import Tuples.{given, *}

  val tests: Tests = Tests {
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

    type `abc diff bc`  = `{a,b,c}` Difference `{b,c}`
    type `abc diff ab`  = `{a,b,c}` Difference `{a,b}`
    type `abcd diff bc` = `{a,b,c,d}` Difference `{b,c}`

    test("elementByType should extract proper element from tuple") {
      assert((1, "2", true).elementByType[Boolean])
      assert((1, "2", true).elementByType[String] == "2")
      assert((1, "2", true).elementsTo("", false) == ("2", true))
    }
    test("setEquals should check that tuple contains all expected types") {
      assertTrue[Equal[`{a,b}`, `{b,a}`]]
      setEquals[`{a,b}`].apply(Set(a, b))
      setEquals[`{a,b}`]((b, a))
      setEquals[`{a,b}`]((b, a))
    }
    test("setIsASuperset should check that tuple contains at least the expected types") {
      setIsASuperset[`{a,b}`](Set(b, a, c))
    }
    
    test("elementByType should extract proper element from tuple") {
      // import Tuples.{given}
      // import org.primetalk.tuple.Tuples.construct
      // val cc = construct[a, ∅, `{a,b,c}`]//(using constructEmpty[`{a,b,c}`])
      // val ccb = construct[a, `{b}`, `{a,b,c}`](using construct[b, ∅, `{a,b,c}`])
      // val fa = compatible[`{a,b,c}`, a *: ∅](using
      //   construct[a, ∅, `{a,b,c}`](using constructEmpty[`{a,b,c}`])
      // )
      // val fab = compatible[`{a,b,c}`, `{a,b}`](using
      //   construct[a, `{b}`, `{a,b,c}`](using
      //     construct[b, ∅, `{a,b,c}`](using constructEmpty[`{a,b,c}`])
      //   )
      // )
      // val f = compatible[`{a,b,c}`, `{a,b}`]
      // assert(f((a, b, c)) == (a, b))
      // val g = compatible[Set3[Int, String, Boolean], Set2[String, Boolean]]
      // assert(g((1, "2", true)) == ("2", true))
      // val converter: (Int, String, Boolean) => (String, Boolean) = summon[(Int, String, Boolean) => (String, Boolean)]
    }

    test("Conversion should automatically convert sets") {
      def foo: `{a,b,c}` = ???
      // val abc: Set3[c,b,a] = foo.c

    }

  }
}
