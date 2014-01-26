
package scala.plmrf

import scala.collection.mutable.Stack
import org.scalatest.FunSuite

class PLMRF_Main_Test extends FunSuite {

  test("ArgumentParse should get Map like below ver train") {
    val opt1 = Main.ArgumentParse(Map(), List("train", "file"))
    assert(opt1.contains("mode"))
    assert(opt1.get("mode") == Some("train"))
    assert(opt1.contains("inputfile"))
    assert(opt1.get("inputfile") == Some("file"))
  }

  test("ArgumentParse should get Map like below ver decode") {
    val opt2 = Main.ArgumentParse(Map(), List("decode", "file"))
    assert(opt2.contains("mode"))
    assert(opt2.get("mode") == Some("decode"))
    assert(opt2.contains("inputfile"))
    assert(opt2.get("inputfile") == Some("file"))
  }

  test("ArgumentParse should get Map like below ver error type1") {
    val opt2 = Main.ArgumentParse(Map(), List())
    assert(opt2 == Map())
  }

  test("ArgumentParse should get Map like below ver error type2") {
    val opt2 = Main.ArgumentParse(Map(), List("hoge", "hoge"))
    assert(opt2.contains("mode"))
    assert(opt2.get("mode") == Some(""))
    assert(!opt2.contains("inputfile"))
  }

  test("Main should run main") {
    expectResult()(scala.plmrf.Main.main(Array.empty))
  }

}
