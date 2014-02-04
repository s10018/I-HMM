
package scala.ihmm

import scala.collection.mutable.Stack
import org.scalatest.FunSuite

class PLMRF_DirRand_Test extends FunSuite {
  test("if 10 given, the size of returned must be 10") {
    val n = DirRand.random(10, 0.2)
    assert (n.size == 10)
  }
}
