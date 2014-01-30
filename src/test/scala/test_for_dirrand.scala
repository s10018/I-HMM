
package scala.ihmm

import scala.collection.mutable.Stack
import org.scalatest.FunSuite

class PLMRF_DirRand_Test extends FunSuite {
  test("the sum of returned must be 1.0") {
    val n = DirRand.random(10, 0.2)
    assert (n.sum == 1.0)
  }
  test("if 10 given, the size of returned must be 10") {
    val n = DirRand.random(10, 0.2)
    assert (n.size == 10)
  }
}
