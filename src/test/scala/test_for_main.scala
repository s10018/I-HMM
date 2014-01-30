
package scala.ihmm

import scala.collection.mutable.Stack
import org.scalatest.FunSuite

class PLMRF_Main_Test extends FunSuite {

  test("ArgumentParse should get Map like below ver error type1") {
    val opt2 = Main.ArgumentParse(Map(), List())
    assert(opt2 == Map())
  }

  test("ArgumentParse should get Map like below ver error type2") {
    val opt2 = Main.ArgumentParse(Map(), List("hoge", "hoge"))
    assert(opt2.contains("mode"))
    assert(opt2.get("mode") == Some(""))
  }

  test("Main should run main") {
    expectResult()(Main.main(Array.empty))
  }

}
