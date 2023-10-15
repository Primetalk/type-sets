package org.primetalk.tyset

import org.primetalk.tyset.TySets._
import utest._

class PortsTest extends TestSuite {

  val tests: Tests = Tests {

    type `{80}` = Singleton[80]
    type `{8080}` = Singleton[8080]
    type `{443}` = Singleton[443]

    type HttpPorts = `{80}` ∪ `{8080}` ∪ `{443}`

    inline def httpPort[P <: Int](i: P)(using ev: BelongsTo[P, HttpPorts]): P = i

    test("80 is an http port") {
      val p = httpPort[80](80)
      assert(p == 80)
    }

  }
}